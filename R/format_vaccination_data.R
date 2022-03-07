#' Title
#'
#' @param ecdc_vaccination_data
#'
#' @return
#' @export
#'
#' @examples
format_vaccination_data <- function(ecdc_vaccination_data){

  # Remove DE, NL and LI contries that have no vaccination data per age groups
  # Keep Region == country_code to remove regional info provided for some countries
  # Add a full_vaccination colum corresponding to 2nd dose unless vaccine == JANSS
  # Keep info by age groups
  # Subset columns of interest and rename TargetGroup
  ecdc_vaccination_data <- ecdc_vaccination_data %>%
    mutate(year_week = sub(pattern = "W", x = YearWeekISO, replacement = '')) %>%
    rename(country_code = ReportingCountry) %>%
    filter(country_code != "NL" & country_code != "DE" & country_code != "LI") %>%
    filter(Region == country_code) %>%
    select(year_week, FirstDose, SecondDose, DoseAdditional1, country_code, Region, Population, Denominator, TargetGroup, Vaccine) %>%
    mutate(full_vaccination = ifelse(Vaccine == "JANSS", FirstDose, SecondDose)) %>%
    mutate(boost = DoseAdditional1) %>%
    filter(TargetGroup == "Age<18" | TargetGroup == "Age18_24" | TargetGroup == "Age25_49"|
             TargetGroup == "Age50_59" | TargetGroup == "Age60_69" | TargetGroup == "Age70_79" |
             TargetGroup == "Age80+") %>%
    mutate(TargetGroup = case_when(TargetGroup == "Age<18" ~ "age_0_17" ,
                                   TargetGroup == "Age18_24" ~ "age_18_24",
                                   TargetGroup == "Age25_49" ~ "age_25_49",
                                   TargetGroup == "Age50_59" ~ "age_50_59",
                                   TargetGroup == "Age60_69" ~ "age_60_69",
                                   TargetGroup == "Age70_79" ~ "age_70_79",
                                   TargetGroup == "Age80+" ~ "age_80_100"))
  return(ecdc_vaccination_data)
}
