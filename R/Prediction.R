#' Simulator: Predict the variant
#'
#' @param data
#' @param start
#' @param end
#'
#' @return
#' @export
#' @import tidyverse
#' @import class
#' @examples
#'
#'
Prediction <- function(data,start = "2021-01",end = "2022-01",list_country_code){


  alldata <- read_csv("data/alldata.csv")

  data = data %>% filter(year_week > start & year_week<end)

  # List of all country with corresponding country code
  #list_country_code <- data %>% select(country_code) %>% distinct %>% filter(country_code != "BG")

  # For each country
  stat_allcountry <- data.frame()
  for (country in list_country_code$country_code) {

    # Data train
    data_country <- alldata %>% filter(country_code == country)
    data_country$year_week_num <- as.numeric(factor(data_country$year_week))
    data_country$year_week_jiter <- jitter(data_country$year_week_num,factor = 0.1)

    # Data test
    test_country <- data %>% filter(country_code == country) %>% select(country_code, year_week)
    test_country$year_week_num <- as.numeric(factor(test_country$year_week,levels = levels(factor(data_country$year_week))))
    test_country$year_week_jiter <- jitter(test_country$year_week_num,factor = 0.1)

    # Prediction with KNN model
    pr <- knn(train = data_country$year_week_jiter,test = data.frame(year_week = test_country$year_week_jiter),cl=data_country$variant,k=1)

    # Create variant variable
    test_country$variant = as.character(pr)

    if("age_group"%in%names(data)){
      test_country$age_group = (data %>% filter(country_code == country))$age_group

      # Concatenate prediction file for all countries
      stat_country= test_country %>% mutate(country_code = country) %>% group_by(country_code,year_week,age_group,variant) %>% summarise(nb = n()) %>% filter(variant != "NA")
      stat_allcountry <- union_all(stat_allcountry, stat_country) %>% group_by(country_code,year_week,age_group,variant,nb)
      write.csv(stat_country,paste0("results/prediction/",country,"_stat_country.csv"))
    } else {
      stat_hospitalisation = test_country %>% group_by(country_code,year_week,variant) %>% summarise(nb = n())
      stat_allcountry_hospitalisation = union_all(stat_allcountry_hospitalisation,stat_hospitalisation)%>% group_by(country_code,year_week,variant,nb)
      write.csv(stat_hospitalisation,paste0("results/prediction/",country,"_stat_hospitalisation.csv"))

    }
  }

  # Save prediction file all countries
  if("age_group"%in%names(data)){
    write.csv(stat_allcountry,"results/prediction/stat_allcountry.csv")
    return(stat_allcountry)
  }else{
    write.csv(stat_allcountry_hospitalisation,"results/prediction/pred_hospitalisation_variant.csv")
    return(stat_allcountry_hospitalisation)
  }


}
