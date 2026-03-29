### Code created with the help of OpenAI's GPT-4
# ============================================================
# Age Structure Module for Indus Village Model
# ============================================================

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

# ------------------------------------------------------------
# 1. Parse NetLogo-style age list strings
# ------------------------------------------------------------
parse_age_list <- function(age_string) {
  age_string %>%
    gsub("\\[|\\]", "", .) %>%
    trimws() %>%
    strsplit("\\s+") %>%
    unlist() %>%
    as.numeric()
}

# ------------------------------------------------------------
# 2. Convert simulation output to tidy format
# ------------------------------------------------------------
tidy_age_data <- function(df,
                          age_breaks = seq(0, 80, 5),
                          normalize = FALSE) {
  all_bins <- cut(age_breaks[-length(age_breaks)],
    breaks = age_breaks,
    right = FALSE,
    include.lowest = TRUE
  )

  tidy <- df %>%
    mutate(run_id = row_number()) %>%
    select(run_id, menAgeStructure, womenAgeStructure) %>%
    pivot_longer(
      cols = c(menAgeStructure, womenAgeStructure),
      names_to = "sex",
      values_to = "age_list"
    ) %>%
    mutate(
      sex = ifelse(sex == "menAgeStructure", "Male", "Female"),
      ages = map(age_list, parse_age_list)
    ) %>%
    unnest(ages) %>%
    mutate(age_bin = cut(ages,
      breaks = age_breaks,
      right = FALSE,
      include.lowest = TRUE
    )) %>%
    count(run_id, sex, age_bin, name = "count") %>%
    # 🔥 THIS LINE FIXES EVERYTHING
    complete(run_id, sex, age_bin = all_bins, fill = list(count = 0))

  if (normalize) {
    tidy <- tidy %>%
      group_by(run_id) %>%
      mutate(count = count / sum(count)) %>%
      ungroup()
  }

  return(tidy)
}

# ------------------------------------------------------------
# 3. Aggregate across simulation runs
# ------------------------------------------------------------
aggregate_pyramid <- function(tidy_df) {
  tidy_df %>%
    group_by(sex, age_bin) %>%
    summarise(
      mean = mean(count),
      median = median(count),
      q25 = quantile(count, 0.25),
      q75 = quantile(count, 0.75),
      min = min(count),
      max = max(count),
      .groups = "drop"
    )
}

# ------------------------------------------------------------
# 4. Prepare for pyramid plotting
# ------------------------------------------------------------
prepare_pyramid_plot <- function(agg_df) {
  agg_df %>%
    mutate(
      mean_plot   = ifelse(sex == "Male", -mean, mean),
      median_plot = ifelse(sex == "Male", -median, median),
      q25_plot    = ifelse(sex == "Male", -q25, q25),
      q75_plot    = ifelse(sex == "Male", -q75, q75),
      min_plot    = ifelse(sex == "Male", -min, min),
      max_plot    = ifelse(sex == "Male", -max, max)
    )
}

# ------------------------------------------------------------
# 5. Plot function
# ------------------------------------------------------------
plot_population_pyramid <- function(plot_df,
                                    show_mean = TRUE,
                                    show_range = FALSE,
                                    show_legend = TRUE) {
  plot_df <- plot_df %>%
    arrange(sex, age_bin) %>%
    mutate(age_bin = factor(age_bin, levels = levels(age_bin)))

  p <- ggplot(plot_df, aes(y = age_bin, group = sex)) +
    geom_ribbon(
      aes(xmin = q25_plot, xmax = q75_plot, fill = sex),
      alpha = 0.3
    ) +
    geom_line(
      aes(x = median_plot, color = sex, group = sex),
      linewidth = 1
    )

  if (show_mean) {
    p <- p +
      geom_line(
        aes(x = mean_plot, color = sex, group = sex),
        linetype = "dashed",
        linewidth = 0.8
      )
  }

  if (show_range) {
    p <- p +
      geom_ribbon(
        aes(xmin = min_plot, xmax = max_plot, fill = sex),
        alpha = 0.1
      )
  }

  if (!show_legend) {
    p <- p +
      guides(fill = FALSE)
  }

  p +
    scale_x_continuous(labels = abs) +
    theme_minimal()
}

# ------------------------------------------------------------
# 6. Wrapper pipeline - single pyramid
# ------------------------------------------------------------
build_population_pyramid <- function(df,
                                     age_breaks = seq(0, 80, 5),
                                     normalize = FALSE) {
  tidy <- tidy_age_data(df, age_breaks, normalize)
  agg <- aggregate_pyramid(tidy)
  plotd <- prepare_pyramid_plot(agg)

  return(plotd)
}

# ------------------------------------------------------------
# 6. Wrapper pipeline - difference between two pyramids
# ------------------------------------------------------------
diff_population_pyramids <- function(pyramid_1, pyramid_2) {
  pyramid_1 %>%
    select(sex, age_bin, median_plot) %>%
    rename(median_1 = median_plot) %>%
    left_join(
      pyramid_2 %>%
        select(sex, age_bin, median_plot) %>%
        rename(median_2 = median_plot),
      by = c("sex", "age_bin")
    ) %>%
    mutate(diff = median_1 - median_2)
}

plot_diff_population_pyramids <- function(diff_df) {
  ggplot(diff_df, aes(x = diff, y = age_bin, fill = sex)) +
    geom_col() +
    scale_x_continuous(labels = abs) +
    labs(
      title = "Difference Pyramid (Matri - Patri)",
      x = "Difference in population",
      y = "Age group"
    ) +
    theme_minimal()
}
# Interpretation:
# Positive = matri-heavy cohort
# Negative = patri-heavy

# ------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------
plot_variability <- function(plot_df) {
  plot_df %>%
    mutate(iqr = abs(q75 - q25)) %>%
    ggplot(aes(x = sex, y = age_bin, fill = iqr)) +
    geom_tile() +
    scale_fill_viridis_c(name = "IQR") +
    labs(
      title = "Variability Across Age-Sex Bins",
      x = "",
      y = "Age group"
    ) +
    theme_minimal()
}

sample_runs <- function(df, n = 50, age_breaks = seq(0, 80, 5), normalize = FALSE) {
  tidy_df <- tidy_age_data(df, age_breaks, normalize)
  sampled_ids <- sample(unique(tidy_df$run_id), n)
  tidy_df %>% filter(run_id %in% sampled_ids)
}

plot_sample_runs <- function(sampled_data) {
  ggplot(sampled_data, aes(x = count, y = age_bin, group = run_id, color = sex)) +
    geom_line(alpha = 0.1) +
    coord_flip()
}
