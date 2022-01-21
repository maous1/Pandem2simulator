#' Title
#'
#' @param daily
#'
#' @return
#' @export
#'
#' @examples
format_daily <- function(daily,local_region){

names(daily) <- daily[1,]
daily <- daily[2:dim(daily)[1],]
daily <- daily %>%
  select(-c(`N cases, WHO label`)) %>%
  rename(Date = `Date / Week`)%>%
  rename(country = `NUTS2/3/country`) %>%
  rename(variant=`N cases, Pango linage`) %>%
  rename(total=`Sample size`)%>%
  rename(sequenced=`N cases`)


daily$Date <- as.numeric(daily$Date)
daily$sequenced <- as.numeric(daily$sequenced)
daily$total <- as.numeric(daily$total)

daily <- daily %>%
  filter(sequenced > 0 ) %>%
  mutate(Date = as.Date(Date, origin = "1899-12-30")) %>%
  mutate(temp = strptime(Date,format="%Y-%m-%d")) %>%
  mutate(temp2 = format(temp,format="%Y")) %>%
  mutate(temp3 = format(temp,format="%U")) %>%
  mutate(year_week = paste(temp2,temp3,sep="-")) %>%
  select( year_week , Date, country , variant , total , sequenced)


no_sequenced <- daily %>%
  group_by(year_week,country) %>%
  summarise(new_cases=total-sum(sequenced)) %>%
  distinct() %>%
  mutate(variant="NSQ")
sequenced <- daily %>%
  rename(new_cases = sequenced) %>%
  select(year_week,country,new_cases,variant)


country <- local_region %>%
  filter(Level == "Country") %>%
  select(Name,Code) %>%
  rename(country= Name) %>%
  rename(country_code=Code)

daily_format <- union_all(sequenced,no_sequenced) %>%
  left_join(country, "country") %>%
  filter(new_cases>0)


return(daily_format)
}
