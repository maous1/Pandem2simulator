#' Title
#'
#' @param case_variants_aggregated
#' @param vaccination_data_formatted
#' @param vaccine_rates
#' @param booster_rates
#' @param start_date
#' @param end_date
#' @param country
#' @param vaccination_correction_factors
#'
#' @return
#' @export
#'
#' @examples
vaccination_simulator <- function(case_variants_aggregated, vaccination_data_formatted, vaccine_rates,
                                  booster_rates, start_date = NULL, end_date = NULL, country = NULL, vaccination_correction_factors = NULL) {
  if (is.null(start_date)) {
    start_date <- min(as.vector(case_variants_aggregated$time))
  }
  if (is.null(end_date)) {
    end_date <- max(as.vector(case_variants_aggregated$time))
  }
  if (!is.null(country)) {
    case_variants_aggregated <- case_variants_aggregated %>%
      filter(country_code %in% country)
  }

  # If no vaccinaion/boster correction factor are given, do not correct for that!
  if (is.null(vaccination_correction_factors)) {
    vaccination_correction_factors <- tibble(year = c(2022, 2021), booster = c(1, 1), vaccine = c(1, 1))
  }

  # Add a fictive age to be able to merge age groups that are different in cases and in vaccination data
  # => conversion column called "vacc_age_group"
  groups <- case_variants_aggregated %>%
    group_by(age_group) %>%
    summarize(n = sum(nb))
  case_variants_age <- tibble()
  for (group in groups$age_group) {
    n_cases <- groups %>%
      filter(age_group == group) %>%
      pull(n)
    tmp <- expandRows(case_variants_aggregated %>% filter(age_group == group), count = "nb", drop = TRUE) %>%
      mutate(age = case_when(
        group == "<15yr" ~ sample(0:14, n_cases, replace = TRUE),
        group == "15-24yr" ~ sample(15:24, n_cases, replace = TRUE),
        group == "25-49yr" ~ sample(25:49, n_cases, replace = TRUE),
        group == "50-64yr" ~ sample(50:64, n_cases, replace = TRUE),
        group == "65-79yr" ~ sample(65:79, n_cases, replace = TRUE),
        group == "80+yr" ~ sample(80:100, n_cases, replace = TRUE)
      )) %>%
      mutate(vacc_age_group = case_when(
        age < 18 ~ "age_0_17",
        age >= 18 & age < 25 ~ "age_18_24",
        age >= 25 & age < 50 ~ "age_25_49",
        age >= 50 & age < 60 ~ "age_50_59",
        age >= 60 & age < 70 ~ "age_60_69",
        age >= 70 & age < 80 ~ "age_70_79",
        age >= 80 ~ "age_80_100"
      ))
    case_variants_age <- rbind(case_variants_age, tmp)
  }

  case_variants_age_vaccine <- case_variants_age %>%
    left_join(vaccine_rates %>% select(country_code, year_week, vacc_age_group, vacc_rate))
  case_variants_age_vaccine[is.na(case_variants_age_vaccine$vacc_rate), "vacc_rate"] <- 0

  # ! keep stratification by age_group (initial age groups!)
  groups <- case_variants_age_vaccine %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, variant, vacc_rate) %>%
    count()

  vax_unvax <- groups %>%
    mutate(n_vax = round(n * vacc_rate), n_unvax = n - n_vax)

  vax_long <- expandRows(vax_unvax %>% ungroup() %>% select(-c(vacc_rate, n, n_unvax)), count = "n_vax", drop = TRUE) %>%
    mutate(vaccination_status = "vaccinated")

  unvax_long <- expandRows(vax_unvax %>% ungroup() %>% select(-c(vacc_rate, n, n_vax)), count = "n_unvax", drop = TRUE) %>%
    mutate(vaccination_status = "unvaccinated")

  case_variants_age_vaccination <- rbind(vax_long, unvax_long)

  rm(vax_unvax)
  rm(vax_long)
  rm(unvax_long)

  case_variants_age_boost <- case_variants_age_vaccination %>%
    left_join(booster_rates %>% rename(vacc_age_group = boost_age_group) %>%
      select(country_code, year_week, vacc_age_group, boost_rate))
  case_variants_age_boost[is.na(case_variants_age_boost$boost_rate), "boost_rate"] <- 0

  # ! keep stratification by age_group (initial age groups!)
  groups <- case_variants_age_boost %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, variant, vaccination_status, boost_rate) %>%
    count()

  # The boost rates refers to vaccinated + unvaccinated population size (per age group per date)
  # evaluate it (n_vax_unvax) and calculate the number of cases corresponding to boosted people
  # This number will have to be ventilated among vaccinated people!
  # !!! as we keep the stratification by age_group (initial age groups!), it is necessary to
  # calculate the number of vaccinated + unvaccinated population size (per initial age_group per vacc_age_group and per variant)!!!
  groups_size <- case_variants_age_boost %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, variant) %>%
    summarize(n_vax_unvax = n())

  groups <- groups %>% left_join(groups_size)

  # n_boost = number of boosted among the age group pop (must be vaccinated to be be boosted!)
  # n_vax_unboost = number of unboosted among the vaccinated people at this age group pop (must be vaccinated to be be boosted!)
  # n_unvax_unboost = number of unvaccinated (already in the table, correponds to the n column of the unvaccinated group at this age category)
  boost_unboost <- groups %>%
    mutate(
      n_boost = ifelse(vaccination_status == "vaccinated", round(n_vax_unvax * boost_rate), 0),
      n_vax_unboost = ifelse(vaccination_status == "vaccinated", n - n_boost, 0)
    )

  boost_long <- expandRows(boost_unboost %>% ungroup() %>%
    select(-boost_rate, -n, -n_vax_unvax, -n_vax_unboost), count = "n_boost", drop = TRUE) %>%
    mutate(booster_status = "boosted")

  vax_unboost_long <- expandRows(boost_unboost %>% ungroup() %>%
    select(-boost_rate, -n, -n_vax_unvax, -n_boost), count = "n_vax_unboost", drop = TRUE) %>%
    mutate(booster_status = "vaccinated")

  unvax_long <- expandRows(boost_unboost %>% ungroup() %>% filter(vaccination_status == "unvaccinated") %>%
    select(-boost_rate, -n_boost, -n_vax_unvax, -n_vax_unboost), count = "n", drop = TRUE) %>%
    mutate(booster_status = "unvaccinated")

  cases_variant_boost_status <- rbind(boost_long, vax_unboost_long, unvax_long)

  rm(boost_long)
  rm(vax_unboost_long)
  rm(unvax_long)


  vaccine_CF_2021 <- vaccination_correction_factors %>%
    filter(year == 2021) %>%
    pull(vaccine)
  vaccine_CF_2022 <- vaccination_correction_factors %>%
    filter(year == 2022) %>%
    pull(vaccine)
  boost_CF_2021 <- vaccination_correction_factors %>%
    filter(year == 2021) %>%
    pull(booster)
  boost_CF_2022 <- vaccination_correction_factors %>%
    filter(year == 2022) %>%
    pull(booster)

  n_cases_per_age <- cases_variant_boost_status %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, variant) %>%
    summarize(pop_size = n())

  cases_variant_vaccination_boost_corrected <- cases_variant_boost_status %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, booster_status, variant) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = booster_status, values_from = n, values_fill = 0) %>%
    pivot_longer(names_to = "booster_status", values_to = "n", -c(country_code, year_week, time, age_group, vacc_age_group, variant)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    mutate(n_boosted_corrected = ifelse(booster_status == "boosted",
      ifelse(year_week <= "2021-52", round(n / boost_CF_2021),
        round(n / boost_CF_2022)
      ), 0
    )) %>%
    mutate(n_vaccinated_corrected = ifelse(booster_status == "vaccinated",
      ifelse(year_week <= "2021-52", round(n / vaccine_CF_2021),
        round(n / vaccine_CF_2022)
      ), 0
    )) %>%
    select(-n)

  cases_variant_unvaccinated_corrected <- cases_variant_boost_status %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, booster_status, variant) %>%
    summarize(n = n()) %>%
    pivot_wider(names_from = booster_status, values_from = n, values_fill = 0) %>%
    pivot_longer(names_to = "booster_status", values_to = "n", -c(country_code, year_week, time, age_group, vacc_age_group, variant)) %>%
    mutate(n = ifelse(is.na(n), 0, n)) %>%
    left_join(n_cases_per_age) %>%
    ungroup() %>%
    mutate(n_boosted_corrected = ifelse(booster_status == "boosted",
      ifelse(year_week <= "2021-52",
        round(n / boost_CF_2021), round(n / boost_CF_2022)
      ), 0
    )) %>%
    mutate(n_vaccinated_corrected = ifelse(booster_status == "vaccinated",
      ifelse(year_week <= "2021-52",
        round(n / vaccine_CF_2021), round(n / vaccine_CF_2022)
      ), 0
    )) %>%
    mutate(cases_to_report = n - n_boosted_corrected - n_vaccinated_corrected) %>%
    group_by(country_code, year_week, time, age_group, vacc_age_group, variant) %>%
    mutate(n_unvax = sum(cases_to_report)) %>%
    filter(booster_status == "unvaccinated") %>%
    select(-c(n, pop_size, n_boosted_corrected, n_vaccinated_corrected, cases_to_report))


  boost_long <- expandRows(cases_variant_vaccination_boost_corrected %>% ungroup() %>%
    select(-n_vaccinated_corrected), count = "n_boosted_corrected", drop = TRUE)

  vax_unboost_long <- expandRows(cases_variant_vaccination_boost_corrected %>% ungroup() %>%
    select(-n_boosted_corrected), count = "n_vaccinated_corrected", drop = TRUE)

  unvax_long <- expandRows(cases_variant_unvaccinated_corrected %>% ungroup(), count = "n_unvax", drop = TRUE)

  cases_variant_vaccination <- rbind(boost_long, vax_unboost_long, unvax_long)

  cases_variant_vaccination$booster_status <- factor(cases_variant_vaccination$booster_status,
    levels = c("unvaccinated", "boosted", "vaccinated")
  )

  cases_variant_vaccination_aggregated <- cases_variant_vaccination %>%
    rename(vaccination_status = booster_status) %>%
    group_by(country_code, year_week, time, age_group, variant, vaccination_status) %>%
    count(vaccination_status) %>%
    ungroup()

  return(cases_variant_vaccination_aggregated)
}
