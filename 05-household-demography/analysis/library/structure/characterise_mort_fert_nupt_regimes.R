characterise_mort_fert_nupt_regimes <- function(endstates) {
    age_structures <- endstates |>

    mutate(
        mortality_regime =
        ifelse(
            cdmlt_level > median(cdmlt_level),
            "Low mortality",
            "High mortality"
        )
    ) |>
    mutate(
        fertility_intensity =
        ifelse(
            c1_fert > median(c1_fert),
            "High fertility",
            "Low fertility"
        )
    ) |>
    mutate(
        fertility_timing =
        ifelse(
            mu_fert > median(mu_fert),
            "Late peak fertility",
            "Early peak fertility"
        )
    ) |>
    mutate(
        fertility_spread_pre_peak =
            ifelse(
            sigma1_fert > median(sigma1_fert),
            "Broad pre-peak fertility",
            "Narrow pre-peak fertility"
            )
    ) |>
    mutate(
        fertility_spread_post_peak =
            ifelse(
            sigma2_fert > median(sigma2_fert),
            "Broad post-peak fertility",
            "Narrow post-peak fertility"
            )
    ) |>
    mutate(
        marriage_timing_women =
            ifelse(
            mu_women > median(mu_women),
            "Late female marriage",
            "Early female marriage"
            )
    ) |>
    mutate(
        marriage_timing_men =
            ifelse(
            mu_men > median(mu_men),
            "Late male marriage",
            "Early male marriage"
            )
    ) |>
    mutate(
        marriage_intensity_women =
            ifelse(
            c1_women > median(c1_women),
            "High women marriage intensity",
            "Low women marriage intensity"
            )
    ) |>
    mutate(
        marriage_intensity_men =
            ifelse(
            c1_men > median(c1_men),
            "High men marriage intensity",
            "Low men marriage intensity"
            )
    ) |>
    mutate(
        marriage_spread_pre_peak_women =
            ifelse(
            sigma1_women > median(sigma1_women),
            "Broad pre-peak marriage spread women",
            "Narrow pre-peak marriage spread women"
            )
    ) |>
    mutate(
        marriage_spread_pre_peak_men =
            ifelse(
            sigma1_men > median(sigma1_men),
            "Broad pre-peak marriage spread men",
            "Narrow pre-peak marriage spread men"
            )
    ) |>
    mutate(
        marriage_spread_post_peak_women =
            ifelse(
            sigma2_women > median(sigma2_women),
            "Broad post-peak marriage spread women",
            "Narrow post-peak marriage spread women"
            )
    ) |>
    mutate(
        marriage_spread_post_peak_men =
            ifelse(
            sigma2_men > median(sigma2_men),
            "Broad post-peak marriage spread men",
            "Narrow post-peak marriage spread men"
            )
    )

    if ("amenorrhea_period_in_days" %in% names(endstates)) {
        age_structures <- age_structures |>
            mutate(
                amenorrhea_length =
                ifelse(
                    amenorrhea_period_in_days > median(amenorrhea_period_in_days),
                    "Long amenorrhea period",
                    "Short amenorrhea period"
                )
            )
    }
    
    age_structures
}