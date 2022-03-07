#' Title
#'
#' @param vaccination_data_formatted
#'
#' @return
#' @export
#'
#' @examples
format_booster_rates <- function(vaccination_data_formatted){

  # Create a table with size population per age_group per country
  # using 'Denominator' that gives size of population per TargetGroups.
  # NB: size of population for TargetGroup == Age0_17 is not given for some countries.
  # => Easy to calculate
  pop_by_age_group_by_country <- vaccination_data_formatted %>%
    select(country_code, Region, TargetGroup, Population, Denominator) %>%
    unique %>%
    select(- Region) %>%
    pivot_wider(names_from = TargetGroup, values_from = Denominator) %>%
    mutate(age_0_17 = Population - (age_18_24 + age_25_49 + age_50_59 + age_60_69 + age_70_79 + age_80_100)) %>%
    select(country_code, age_0_17, age_18_24, age_25_49, age_50_59, age_60_69, age_70_79 ,age_80_100)

  # Evaluate number of vaccines per week, per age group, and per country
  vaccine_per_week_per_age_per_country <- vaccination_data_formatted %>%
    select(country_code, year_week, TargetGroup, Vaccine, full_vaccination) %>%
    group_by(country_code, year_week, TargetGroup) %>%
    summarize(n = sum(full_vaccination)) %>%
    pivot_wider(names_from = TargetGroup, values_from = n, values_fill = 0)

  # Evaluate number of boost per week, per age group, and per country
  boost_per_week_per_age_per_country <- vaccination_data_formatted %>%
    select(country_code, year_week, TargetGroup, Vaccine, boost) %>%
    group_by(country_code, year_week, TargetGroup) %>%
    summarize(n = sum(boost)) %>%
    pivot_wider(names_from = TargetGroup, values_from = n, values_fill = 0)

  boost_rates <- boost_per_week_per_age_per_country %>%
    ungroup() %>%
    group_by(country_code) %>%
    mutate(age_0_17_cs = cumsum(age_0_17),
           age_18_24_cs = cumsum(age_18_24),
           age_25_49_cs = cumsum(age_25_49),
           age_50_59_cs = cumsum(age_50_59),
           age_60_69_cs = cumsum(age_60_69),
           age_70_79_cs = cumsum(age_70_79),
           age_80_100_cs = cumsum(age_80_100)) %>%
    select(country_code, year_week, ends_with("cs")) %>%
    pivot_longer(names_to = "age_group", values_to = "cumulative_sum", -c(country_code, year_week)) %>%
    mutate(boost_age_group = sub("_cs", age_group, replacement = '')) %>%
    left_join(pop_by_age_group_by_country %>%
                pivot_longer(names_to = "boost_age_group", values_to = "pop", -country_code)) %>%
    mutate(boost_rate = cumulative_sum / pop) %>%
    mutate(n = sub(pattern = "age_", x = boost_age_group, replacement = ''),
           age_inf = as.integer(sub(pattern = "_\\d*", x = n, replacement = '')),
           age_sup = as.integer(sub(pattern = "^\\d*_", x = n, replacement = ''))) %>%
    select(country_code, year_week, boost_age_group, age_inf, age_sup, boost_rate) %>%
    mutate(boost_rate = ifelse(boost_rate >1, 1, boost_rate))

  return(boost_rates)
}
