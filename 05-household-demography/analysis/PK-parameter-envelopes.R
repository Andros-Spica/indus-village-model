#### Batch pipeline: OWID fertility -> scale with World Bank TFR -> harmonize -> PK fits -> bootstrap -> empirical envelope
#### Targets: Yemen, Afghanistan, Iraq, Nepal, Laos, Cambodia, Sierra Leone, Niger, Mali, Chad,
#### Central African Republic, Congo, D.R. Congo, Angola, Sudan, Ethiopia, Somalia, Tanzania, Mozambique
#### Period: 1950-1995

# Install / load packages
install_if_missing <- function(pkgs){
  toins <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
  if(length(toins)) install.packages(toins, dependencies=TRUE)
}
install_if_missing(c("readr","dplyr","purrr","WDI","pryr","pracma","readxl","numDeriv","boot","demogR"))
library(readr); library(dplyr); library(purrr); library(WDI); library(pracma)
library(readxl); library(numDeriv); library(boot); library(demogR)

# --- User settings ---
countries <- c("Yemen","Afghanistan","Iraq","Nepal","Laos","Cambodia","Sierra Leone","Niger","Mali",
               "Chad","Central African Republic","Congo","Democratic Republic of the Congo","Angola",
               "Sudan","Ethiopia","Somalia","Tanzania","Mozambique")
# note: some names may need to be adjusted to match OWID/World Bank naming (script tries to match heuristically)
years <- 1950:1995
age_grid <- 15:49   # single-year reproductive ages

# URLs / filenames used
owid_url <- "https://ourworldindata.org/grapher/age-of-mothers-at-childbirth-by-year.csv"
un_marriage_local <- "data/undesa_pd_2019_wmd_marital_status.xlsx"  # optional: place UN excel here if you have it

# --- Helper functions ---
# 1) PK single-hump
pk_one <- function(age, c1, mu, s1, s2){
  s <- ifelse(age <= mu, s1, s2)
  p <- c1 * exp(-0.5 * ((age - mu)/s)^2)
  pmax(0, p)
}

# 2) PCHIP monotone interpolation on log-scale when values non-negative
interp_to_ages <- function(age_in, val_in, age_out){
  ok <- !is.na(age_in) & !is.na(val_in)
  age_in <- age_in[ok]; val_in <- val_in[ok]
  if(length(age_in) < 2) return(rep(NA_real_, length(age_out)))
  # if nonnegative, interpolate log to avoid negative wiggles
  if(all(val_in >= 0)){
    eps <- min(1e-8, min(val_in[val_in>0], na.rm=TRUE)/10)
    val_adj <- val_in
    val_adj[val_adj==0] <- eps
    logv <- log(val_adj)
    f <- splinefun(age_in, logv, method="monoH.FC")
    out <- exp(f(age_out))
    out[out < eps*10] <- 0
  } else {
    f <- splinefun(age_in, val_in, method="monoH.FC")
    out <- f(age_out)
  }
  out
}

# 3) Convert grouped age label like "15-19" -> midpoint (used for OWID age groups)
age_group_midpoint <- function(s){
  s <- trimws(as.character(s))
  if(grepl("\\+", s)){ return(as.numeric(gsub("\\+","",s))) }
  if(grepl("-", s)){
    p <- strsplit(s, "-")[[1]]
    return(mean(as.numeric(p)))
  }
  as.numeric(s)
}

# 4) Read OWID age-of-mothers CSV (share or shape by age)
message("Downloading OWID age-of-mothers CSV (this may take a moment).")
owid_all <- read_csv(owid_url, show_col_types = FALSE)
names(owid_all) <- tolower(names(owid_all))
# Normalize
cn <- names(owid_all)
age_col <- cn[grepl("year", cn)][1]
entity_col <- cn[grepl("entity|country|location", cn)][1]
value_col <- cn[grepl("value|share|proportion|percent", cn)][1]
if(is.null(age_col) | is.null(entity_col) | is.null(value_col)){
  stop("OWID CSV layout unexpected. Inspect column names: ", paste(cn, collapse=", "))
}
owid_all <- owid_all %>%
  rename(age_group = !!age_col, entity = !!entity_col, value = !!value_col) %>%
  mutate(age_mid = sapply(age_group, age_group_midpoint))
# Now, OWID file columns expected: entity, year, age (age_group), value !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# 5) Get TFR from World Bank using WDI package (indicator SP.DYN.TFRT.IN)
#    We'll fetch the whole 1950-1995 window for the countries of interest.
# Map country names to WDI names if necessary. We'll attempt automatic matching; WDI has its own country list.
wdi_countries <- WDI::WDI_data$country %>% as.data.frame()
# helper: best fuzzy match of name to WDI country name
match_country_wdi <- function(name){
  # direct match
  if(name %in% wdi_countries$country){
    return(wdi_countries$iso2c[wdi_countries$country == name][1])
  }
  # try few heuristics
  alt <- name
  alt <- gsub("Democratic Republic of the Congo", "Congo, Dem. Rep.", alt)
  alt <- gsub("Congo", "Congo, Rep.", alt) # user gave 'Congo' ambiguous; adjust if needed
  # try contains match
  idx <- grep(tolower(gsub("[^a-z]","",name)), tolower(gsub("[^a-z]","",wdi_countries$country)))
  if(length(idx)>=1) return(wdi_countries$iso2c[idx[1]])
  return(NA_character_)
}

# Pull TFR time series for each target (1950-1995)
country_iso2 <- sapply(countries, match_country_wdi, USE.NAMES=FALSE)
# Where iso2 is NA, try manual mappings for special names:
manual_map <- list("Democratic Republic of the Congo"="CD","Congo"="CG","Congo, Rep."="CG",
                   "Central African Republic"="CF")
for(i in seq_along(countries)){
  if(is.na(country_iso2[i])){
    nm <- countries[i]
    if(nm %in% names(manual_map)) country_iso2[i] <- manual_map[[nm]]
  }
}
# fetch
message("Downloading TFR from World Bank (WDI) for selected countries 1950-1995.")
wdi_res <- WDI(country = country_iso2[!is.na(country_iso2)],
               indicator = "SP.DYN.TFRT.IN",
               start = min(years), end = max(years), extra = FALSE, cache = NULL)
# wdi_res: country, iso2c, year, SP.DYN.TFRT.IN
names(wdi_res)[names(wdi_res)=="SP.DYN.TFRT.IN"] <- "TFR"

# --- Prepare outputs ----
out_dir <- "pk_empirical_outputs"
if(!dir.exists(out_dir)) dir.create(out_dir)

# helper to scale OWID shares -> ASFR given TFR: ASFR(age) such that sum_age ASFR(age) == TFR
scale_shape_to_asfr <- function(shape_vec, tfr){
  if(is.null(tfr) || is.na(tfr)) return(NA_real_)
  if(sum(shape_vec, na.rm=TRUE) <= 0) return(rep(NA_real_, length(shape_vec)))
  asfr <- shape_vec / sum(shape_vec) * tfr
  asfr
}

# function to compute realized ASFR given p_ever_married_by_age and p_birth_if_married(age)
hazard_to_p_ever <- function(hazard, ages){
  p_not_married <- cumprod(1 - pmin(0.999, hazard))
  1 - p_not_married
}

# PK fit function (nls) with safe guards
fit_pk_one_safe <- function(ages, rates, start = list(c1=0.12, mu=28, s1=3, s2=6)){
  df <- data.frame(age=ages, rate=rates)
  df$rate[df$rate <= 0] <- 1e-8
  res <- try(nls(rate ~ pk_one(age, c1, mu, s1, s2), data = df, start = start,
                 control = nls.control(maxiter=500, warnOnly=TRUE)), silent=TRUE)
  if(inherits(res,"try-error")) return(NULL)
  list(fit = res, coef = coef(res))
}

# bootstrap residuals for an nls fit to get parameter samples
bootstrap_nls_params <- function(nls_fit, ages, rates, B=300, seed=123){
  set.seed(seed)
  resid0 <- residuals(nls_fit)
  fitted0 <- fitted(nls_fit)
  pnames <- names(coef(nls_fit))
  out <- matrix(NA, nrow=B, ncol=length(pnames))
  colnames(out) <- pnames
  for(b in 1:B){
    rstar <- sample(resid0, replace=TRUE)
    ystar <- fitted0 + rstar
    dfstar <- data.frame(age = ages, rate = ystar)
    fstar <- try(nls(rate ~ pk_one(age, c1, mu, s1, s2), data = dfstar,
                     start = as.list(coef(nls_fit)), control = nls.control(maxiter=300, warnOnly=TRUE)), silent=TRUE)
    if(!inherits(fstar,"try-error")) out[b,] <- coef(fstar)
  }
  as.data.frame(out[complete.cases(out),,drop=FALSE])
}

# --- Main loop: for each country & year produce ASFR and fit PK ---
all_fits <- list()
all_boots <- list()
for(country in countries){
  message("Processing country: ", country)
  # attempt to find OWID entity name exact or by fuzzy contains
  ents <- unique(owid_all$entity)
  ent_match <- NULL
  if(country %in% ents) ent_match <- country
  else {
    # try fuzzy contains
    idx <- grep(tolower(gsub("[^a-z]","",country)), tolower(gsub("[^a-z]","",ents)))
    if(length(idx)>0) ent_match <- ents[idx[1]]
  }
  if(is.null(ent_match)){
    message("  No OWID data entity matched for ", country, " — skipping OWID-based fertility (you can add a manual file).")
    next
  }
  # subset OWID for that entity and years
  sub <- owid_all %>% filter(entity == ent_match & year %in% years)
  if(nrow(sub)==0){
    message("  No OWID rows for matched entity in years 1950-1995 — skipping.")
    next
  }
  # iterate years (some years missing; we will process only available years)
  yrs <- sort(unique(sub$year))
  for(yr in yrs){
    if(!(yr %in% years)) next
    subyr <- sub %>% filter(year == yr)
    # convert OWID grouped shares -> single-year shape on age_grid
    ages_in <- subyr$age_mid
    vals_in <- subyr$value
    shape_single <- interp_to_ages(ages_in, vals_in, age_grid)
    # if shape sums to 0 or NA, skip
    if(all(is.na(shape_single)) || sum(shape_single, na.rm=TRUE) <= 0){
      message("  ", country, " ", yr, ": no usable OWID shape -> skipping.")
      next
    }
    # get TFR for this country-year from wdi_res
    # find iso2 code for this country match
    iso2 <- match_country_wdi(country)
    tfr_row <- wdi_res %>% filter(iso2c == iso2 & year == yr)
    tfr_val <- if(nrow(tfr_row)>0) tfr_row$TFR[1] else NA_real_
    # if TFR missing, try choose nearest year available in WDI
    if(is.na(tfr_val) || is.nan(tfr_val)){
      tfr_near <- wdi_res %>% filter(iso2c == iso2) %>% arrange(abs(year - yr))
      if(nrow(tfr_near)>0) tfr_val <- tfr_near$TFR[1] else tfr_val <- NA_real_
    }
    # produce ASFR shape: normalized shape -> scale to TFR if available
    normalized_shape <- shape_single / sum(shape_single, na.rm=TRUE)
    if(!is.na(tfr_val)){
      asfr <- scale_shape_to_asfr(normalized_shape, tfr_val)
    } else {
      # no TFR: keep normalized shape and mark flagged; user can scale later
      asfr <- normalized_shape
    }
    # save ASFR to CSV
    outname <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9]","_",country), "_", yr, "_ASFR.csv"))
    write.csv(data.frame(age=age_grid, asfr=asfr), outname, row.names=FALSE)
    # Now get marriage info:
    # Try to read UN World Marriage Data if provided locally, otherwise synthesize from default median ages
    marriage_proportion_by_age <- NULL
    if(file.exists(un_marriage_local)){
      message("  Reading UN marriage data file (local) to obtain age-specific proportion married if available.")
      # attempt to parse Excel: this is necessarily heuristic because UN file layouts vary
      un_df <- read_excel(un_marriage_local, sheet = 1)
      un_cols <- tolower(names(un_df))
      # try to find rows for the country & year and columns with age groups
      # (this is a best-effort; for robust work load the UN file manually and adapt this block)
      # fallback: synthesize below
    }
    # fallback synthesis: use median age at first marriage if you have it; otherwise pick region-typical medians
    # We'll define a small default median table for these countries in 1950-1995 (approx early marriage)
    default_median <- c(
      "Yemen"=18, "Afghanistan"=17, "Iraq"=20, "Nepal"=18, "Laos"=19, "Cambodia"=18,
      "Sierra Leone"=17, "Niger"=17, "Mali"=17, "Chad"=17, "Central African Republic"=17,
      "Congo"=18, "Democratic Republic of the Congo"=17, "Angola"=18, "Sudan"=18,
      "Ethiopia"=17, "Somalia"=17, "Tanzania"=18, "Mozambique"=18
    )
    median_age <- ifelse(country %in% names(default_median), default_median[[country]], 18)
    # create first-marriage hazard shape (annual) centered at median_age and convert to P(ever married by age)
    # peak_height is a tuning parameter: typical annual first-marriage hazard peaks might be around 0.1-0.25 in high-marriage settings
    peak_hazard <- 0.12
    hazard <- dnorm(age_grid, mean = median_age, sd = 3)
    hazard <- hazard / max(hazard, na.rm=TRUE) * peak_hazard
    p_ever <- hazard_to_p_ever(hazard, age_grid)
    # compute conditional p_birth_if_married: use asfr shape / p_ever normalized to a conditional max
    # If we have asfr scaled to TFR, asfr is unconditional; we want to infer p_birth_if_married
    # simplest approach: assume conditional shape proportional to normalized_shape and peak conditional birth probability = 0.25
    peak_cond_birth <- 0.25
    p_birth_if_married <- normalized_shape / max(normalized_shape, na.rm=TRUE) * peak_cond_birth
    # realized unconditional ASFR implied by p_ever and conditional births:
    realized_asfr <- p_ever * p_birth_if_married
    # if we had scaled asfr (i.e., tfr_val present), re-scale conditional birth so realized_asfr sums to observed asfr
    if(!is.na(tfr_val)){
      scalek <- sum(asfr, na.rm=TRUE) / sum(realized_asfr, na.rm=TRUE)
      p_birth_if_married <- p_birth_if_married * scalek
      realized_asfr <- p_ever * p_birth_if_married
    } else {
      # if asfr is only normalized shape, align totals so realized_asfr sums to same total as normalized_shape
      scalek <- sum(normalized_shape, na.rm=TRUE) / sum(realized_asfr, na.rm=TRUE)
      p_birth_if_married <- p_birth_if_married * scalek
      realized_asfr <- p_ever * p_birth_if_married
    }
    # Fit PK to realized_asfr (we treat realized_asfr as observed ASFR)
    fit <- fit_pk_one_safe(age_grid, realized_asfr, start = list(c1=0.12, mu=28, s1=3, s2=5))
    if(is.null(fit)){
      message("  PK fertility fit failed for ", country, " ", yr, " — saving ASFR but no fit.")
      next
    } else {
      # bootstrap parameters
      boots <- bootstrap_nls_params(fit$fit, age_grid, realized_asfr, B=300, seed=123)
      # save fit and boot samples
      fitname <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9]","_",country), "_", yr, "_PKfit.rds"))
      saveRDS(list(country=country, year=yr, fit=fit, boots=boots,
                   asfr = realized_asfr, tfr = tfr_val, p_ever = p_ever, p_birth_if_married = p_birth_if_married),
              fitname)
      all_fits[[paste(country,yr,sep="_")]] <- list(country=country, year=yr, fit=fit$coef, tfr = tfr_val)
      all_boots[[paste(country,yr,sep="_")]] <- boots
    }
  } # end year loop
} # end country loop

# --- Pool bootstraps across all fits to form empirical envelope ---
# bind all bootstrap rows (if exist)
bootstrap_pool <- do.call(rbind, lapply(all_boots, function(x) if(is.data.frame(x)) x else NULL))
if(is.null(bootstrap_pool) || nrow(bootstrap_pool)==0) stop("No bootstrap parameter samples collected; inspect logs.")
# compute 2.5%/97.5% envelopes by parameter
envelope <- apply(bootstrap_pool, 2, quantile, probs=c(0.025,0.975), na.rm=TRUE)
envelope_df <- data.frame(param = rownames(envelope), lower = envelope[1,], upper = envelope[2,], row.names=NULL)
write.csv(envelope_df, file.path(out_dir, "empirical_pk_param_envelope_1950_1995.csv"), row.names=FALSE)
message("Envelope saved to: ", file.path(out_dir, "empirical_pk_param_envelope_1950_1995.csv"))

# Save pooled bootstrap samples for later correlated resampling
saveRDS(bootstrap_pool, file.path(out_dir, "pooled_pk_bootstraps_1950_1995.rds"))

message("Processing complete. Outputs in folder: ", out_dir)
message("Caveats: marriage schedules were synthesized from default median ages where UN marriage data were not provided. Replace these with UN or DHS age-specific marriage data for better empirical envelopes.")
