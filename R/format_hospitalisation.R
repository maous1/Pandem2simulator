#' disaggregate hospitalisation files
#'
#' @param hospitalisation
#'
#' @param vaccination
#' @param case_aggregated
#'
#' @return
#' @export
#' @examples
format_hospitalisation <- function(hospitalisation, vaccination, case_aggregated, datedepart, datefin) {
  date <- format_date_pandem(datedepart = datedepart, datefin = datefin)

  Population <- vaccination %>%
    select(ReportingCountry, Population) %>%
    distinct() %>%
    rename(country_code = ReportingCountry)

  list_country_code <- case_aggregated %>%
    select(ï..country, country_code) %>%
    distinct() %>%
    filter(country_code != "BG")

  hospitalisation_detected <- hospitalisation %>%
    left_join(list_country_code, by = "ï..country") %>%
    filter(indicator == "Weekly new hospital admissions per 100k") %>%
    left_join(Population, by = "country_code") %>%
    group_by(country_code, year_week) %>%
    summarise(new_cases = (value * Population / 100000))


  hospitalisation_detected$year_week <- sub(x = hospitalisation_detected$year_week, pattern = "W", replacement = "")
  hospitalisation_detected <- right_join(x = hospitalisation_detected, y = date, "year_week")
  return(hospitalisation_detected)
}
