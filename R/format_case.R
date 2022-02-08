#' Formatting the files from ecdc :"https://opendata.ecdc.europa.eu/covid19/agecasesnational/csv/data.csv"- and only keep usefull columns
#'
#' @return
#' @export
#' @examples
format_case <- function(case_aggregated) {

  case_aggregated_format <- case_aggregated %>%
    select(country_code,year_week,age_group,new_cases)%>%
    filter(new_cases > 0) %>% rename(time = year_week)

  return(case_aggregated_format)
}
