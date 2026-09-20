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
            "High peak fertility",
            "Low peak fertility"
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
        union_timing_women =
            ifelse(
            mu_women > median(mu_women),
            "Late female union",
            "Early female union"
            )
    ) |>
    mutate(
        union_timing_men =
            ifelse(
            mu_men > median(mu_men),
            "Late male union",
            "Early male union"
            )
    ) |>
    mutate(
        union_intensity_women =
            ifelse(
            c1_women > median(c1_women),
            "High women union peak intensity",
            "Low women union peak intensity"
            )
    ) |>
    mutate(
        union_intensity_men =
            ifelse(
            c1_men > median(c1_men),
            "High men union peak intensity",
            "Low men union peak intensity"
            )
    ) |>
    mutate(
        union_spread_pre_peak_women =
            ifelse(
            sigma1_women > median(sigma1_women),
            "Broad pre-peak union spread women",
            "Narrow pre-peak union spread women"
            )
    ) |>
    mutate(
        union_spread_pre_peak_men =
            ifelse(
            sigma1_men > median(sigma1_men),
            "Broad pre-peak union spread men",
            "Narrow pre-peak union spread men"
            )
    ) |>
    mutate(
        union_spread_post_peak_women =
            ifelse(
            sigma2_women > median(sigma2_women),
            "Broad post-peak union spread women",
            "Narrow post-peak union spread women"
            )
    ) |>
    mutate(
        union_spread_post_peak_men =
            ifelse(
            sigma2_men > median(sigma2_men),
            "Broad post-peak union spread men",
            "Narrow post-peak union spread men"
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