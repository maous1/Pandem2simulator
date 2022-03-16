-#' disaggregate hospitalisation files
  #'
  #' @param hospitalisation
  #'
  #' @param vaccination
  #' @param case_aggregated
  #'
  #' @return disaggregate data in "data/ecdc/hospitalisation.csv"
  #' @export
  #' @examples
  format_hospitalisation <- function(hospitalisation,vaccination,case_aggregated) {


    Population = vaccination %>%
      select(ReportingCountry,Population)%>%
      distinct()%>%
      rename(country_code = ReportingCountry)

    hospitalisation <- read_csv("data/ecdc/hospitalisation.csv")
    data = read_csv("data/ecdc/age_cases.csv")
    list_country_code <- data %>% select(country, country_code) %>% distinct %>% filter(country_code != "BG")

    hospitalisation_detected = hospitalisation %>%
      left_join(list_country_code ,by = "country")%>%
      filter(indicator == "Weekly new hospital admissions per 100k")%>%
      left_join(Population ,by = "country_code")%>%
      group_by(country_code,year_week)%>%
      summarise(n = (value*Population/100000))

    # Create table with one variant per line
    hospitalisation_data <- tibble()
    entries_per_hospitalisation <- function(x){
      country <- x[1]
      yw <- x[2]
      n <- x[3]
      tibble(country_code = rep(country, n),
             year_week = rep(yw, n))
    }

    hospitalisation_data <- bind_rows(apply(hospitalisation_detected, 1, entries_per_hospitalisation))
    hospitalisation_data$year_week = str_replace(hospitalisation_data$year_week, "W", "")

    return(hospitalisation_data)
  }
