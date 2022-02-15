#' Formatting the files from ecdc :"https://opendata.ecdc.europa.eu/covid19/agecasesnational/csv/data.csv"- and only keep usefull columns
#'
#' @return
#' @export
#' @examples
format_case <- function(case_aggregated,datedepart,datefin) {

  date = format_date_pandem(datedepart = datedepart,datefin = datefin)
  case_aggregated <-  right_join(x = case_aggregated,y =  date,"year_week")
  case_aggregated_format <- case_aggregated %>%
    select(country_code,year_week,time,age_group,new_cases)%>%
    filter(new_cases > 0)

  return(case_aggregated_format)
}
