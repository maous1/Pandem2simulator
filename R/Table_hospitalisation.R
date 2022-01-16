#' Title
#'
#' @return
#' @export
#'
#' @examples
Table_hospitalisation <- function() {

  hospitalisation <- read_csv("data/ecdc/hospitalisation.csv")

  hospitalisation_detected = hospitalisation %>%
    left_join(list_country_code ,by = "country")%>%
    filter(year_week >= data_collected_from & indicator == "Weekly new hospital admissions per 100k")%>%
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

  # Save hospitalisation_data
  write_csv(hospitalisation_data,"data/hospitalisation_data.csv")
}
