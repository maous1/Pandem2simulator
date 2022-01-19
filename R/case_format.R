#' disaggregate case_age files
#'
#' @return disaggregate data in "data/ecdc/age_cases.csv"
#' @export
#' @import tidyverse
#' @examples
case_format <- function(case_aggregated) {

  case_aggregated <- case_aggregated %>%
    select(country_code,year_week,age_group,new_cases)%>%
    filter(new_cases > 0)

  return(case_aggregated)
}
