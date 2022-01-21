#' put the files case in the right format
#'
#' @return disaggregate data in "data/ecdc/age_cases.csv"
#' @export
#' @import tidyverse
#' @examples
format_case <- function(case_aggregated) {

  case_aggregated <- case_aggregated %>%
    select(country_code,year_week,age_group,new_cases)%>%
    filter(new_cases > 0)

  return(case_aggregated_format)
}
