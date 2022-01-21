#' format the RIVM files
#'
#' @param case_RIVM
#' @param voc
#'
#' @return
#' @export
#'
#' @examples
format_rivm <- function(case_RIVM,local_region){

  names(case_RIVM) <- case_RIVM[1,]
  case_RIVM <- case_RIVM[2:dim(case_RIVM)[1],]
  case_RIVM <- case_RIVM %>%
    select(Date,`NUTS2/3/country` ,`Confirmed cases`) %>%
    rename(Code_municipality = `NUTS2/3/country`)%>%rename(new_cases = `Confirmed cases`)
  case_RIVM$Date <- as.numeric(case_RIVM$Date)
  case_RIVM$new_cases <- as.numeric(case_RIVM$new_cases)
  case_RIVM <- case_RIVM %>%
    filter(new_cases>0) %>%
    mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
    mutate(temp = strptime(Date,format="%Y-%m-%d")) %>%
    mutate(temp2 = format(temp,format="%Y")) %>%
    mutate(temp3 = format(temp,format="%U")) %>%
    mutate(year_week = paste(temp2,temp3,sep="-")) %>%
    select(Code_municipality, Date, year_week, new_cases)



  country <- local_region%>%
    filter(Level == "Country") %>%
    select(Name,Code) %>%
    rename(country= Name) %>%
    rename(country_code=Code)

  safety <- local_region%>%
    filter(Level == "Safety Region") %>%
    select(Name,Code,Parent) %>%
    rename(Region=Name) %>%
    rename(country_code=Parent) %>%
    rename(Code_region=Code)

  municipaly <- local_region%>%
    filter(Level == "Municipality") %>%
    select(Name,Code,Parent) %>%
    rename(Municipality=Name) %>%
    rename(Code_region=Parent) %>%
    rename(Code_municipality=Code)

  case_RIVM_format <- case_RIVM %>%
    left_join(municipaly,by = "Code_municipality") %>%
    left_join(safety,by = "Code_region") %>%
    left_join(country,by = "country_code")


  return(case_RIVM_format)

}
