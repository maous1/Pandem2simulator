#' disaggregate case_age files
#'
#' @return disaggregate data in "data/ecdc/age_cases.csv"
#' @export
#' @import tidyverse
#' @examples
Table_case <- function() {
  case_ages <- read_csv("data/ecdc/age_cases.csv")

  case_ages_detected = case_ages %>%
    select(country_code,year_week,age_group,new_cases)%>%
    filter(new_cases > 0)

  # Create table with one variant per line
  case_age_data <- tibble()
  entries_per_case <- function(x){
    country <- x[1]
    yw <- x[2]
    var <- x[3]
    n <- x[4]
    tibble(country_code = rep(country, n),
           year_week = rep(yw, n),
           age_group = rep(var, n))
  }
  case_age_data <- bind_rows(apply(case_ages_detected, 1, entries_per_case))

  # Save case_age_data
  write_csv(case_age_data,"data/case_age_data.csv")
  return(case_age_data)
}
