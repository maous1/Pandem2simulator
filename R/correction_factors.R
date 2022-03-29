#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
correction_factors <- function(data) {
  # Apply a correction factor

  # IRR vaccines and boost (all ages / time confounded)
  mean_IRR_vacc_boost <- data %>%
    filter(outcome == "case") %>%
    filter(age_group != "all_ages") %>%
    mutate(year_week = paste0(substr(mmwr_week, 1, 4), "-", substr(mmwr_week, 5, 6))) %>%
    select(outcome, crude_booster_irr, crude_irr) %>%
    dplyr::rename(booster = crude_booster_irr, vaccine = crude_irr) %>%
    group_by(outcome) %>%
    summarize(booster = mean(booster, na.rm = TRUE), vaccine = mean(vaccine, na.rm = TRUE)) %>%
    pivot_longer(names_to = "incidence_ratio", values_to = "irr", -c(outcome))

  mean_vaccine_IRR <- pull(mean_IRR_vacc_boost[mean_IRR_vacc_boost$incidence_ratio == "vaccine", "irr"])
  mean_boost_IRR <- pull(mean_IRR_vacc_boost[mean_IRR_vacc_boost$incidence_ratio == "booster", "irr"])

  # IRR vaccines and boost (all ages / time confounded)
  # The protection factors dropped with omicron
  # => Evaluate 2 correction factors (one "pre-omicron", one for "omicron" period)
  # For simplocity, use only the "pre-omicron" correction factor but this could be changed if needed
  vaccination_correction_factors <- data %>%
    filter(outcome == "case") %>%
    filter(age_group != "all_ages") %>%
    mutate(year_week = paste0(substr(mmwr_week, 1, 4), "-", substr(mmwr_week, 5, 6))) %>%
    select(year_week, outcome, crude_booster_irr, crude_irr) %>%
    mutate(period = case_when(
      year_week < "2021-50" ~ "pre-omicron",
      year_week > "2021-50" ~ "omicron"
    )) %>%
    mutate(year = case_when(
      year_week < "2021-50" ~ 2021,
      year_week > "2021-50" ~ 2022
    )) %>%
    dplyr::rename(booster = crude_booster_irr, vaccine = crude_irr) %>%
    group_by(outcome, period, year, ) %>%
    summarize(booster = mean(booster, na.rm = TRUE), vaccine = mean(vaccine, na.rm = TRUE))
  return(vaccination_correction_factors)
}
