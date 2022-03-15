#' Simulator: Predict the variant from a training set with a column time and a column variant to a testset with a column time
#'
#' @param trainset the data with a column year_week and variant
#' @param testset the data where the variant will be predicted. Need a year_week column
#' @param time
#' @param geolocalisation
#' @param count
#' @param outcome
#' @param factor
#'
#' @return
#' @export
#' @examples
#'
#'
simulator_combine <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL,factor){
  set.seed(2)
  names(trainset)[names(trainset) %in% geolocalisation]<-"geolocalisation"
  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% geolocalisation]<-"geolocalisation"
  names(testset)[names(testset) %in% time]<-"time"

  geo <- unique(trainset$geolocalisation)

  geotrain <- map(.x=geo,.f = function(.x) trainset%>%filter(geolocalisation == .x)%>%group_by(time)%>%
                    mutate(somme = sum(new_cases)) %>%
                    mutate(new_cases = as.integer(round(new_cases/somme*factor)))%>%
                    filter(new_cases >0))
  geotest <- map(.x=geo,.f = function(.x) testset%>%filter(geolocalisation == .x))
  #trainset <- trainset %>% mutate(new_cases = as.integer(round(new_cases/factor))) %>% filter(new_cases >0)
  trainset <- trainset%>%group_by(geolocalisation,time)%>%
    mutate(somme = sum(new_cases)) %>%
    mutate(new_cases = as.integer(round(new_cases/somme*factor)))%>%
    filter(new_cases >0)

  alldata <- map2_df(.x=geotrain,.y=geotest,.f = function(.x,.y) simulator_fragmentation(trainset=.x,testset=.y,time=time,geolocalisation=geolocalisation,outcome=outcome,count=count,factor="month"))
  return(alldata)
}

