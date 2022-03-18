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

  names(trainset)[names(trainset) %in% geolocalisation]<-"geolocalisation"
  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% geolocalisation]<-"geolocalisation"
  names(testset)[names(testset) %in% time]<-"time"

  ####### split the dataframe in a list of dataframe (one per geolocalisation) and keep common geolocalisation

  trainset_list <- trainset %>% split(.$geolocalisation)
  testset_list <- testset %>% split(.$geolocalisation)

  common_geo <- intersect(names(trainset_list),names(testset_list))
  trainset_list <- trainset_list[is.element(names(trainset_list),common_geo)]
  testset_list <- testset_list[is.element(names(testset_list),common_geo)]

  ###### reduce the training dataset

  trainset_reduce <- function(trainset_df,factor)
  {
    trainset_df <- trainset_df%>%
      group_by(time)%>%
      mutate(sum = sum(new_cases)) %>%
      mutate(new_cases = as.integer(round(factor*new_cases/sum)))%>%
      filter(new_cases >0) %>%
      select(-sum)
    return(trainset_df)
  }

  trainset_list <- map(.x=trainset_list,.f = trainset_reduce,factor=factor)

  ###### apply the simulator

  alldata <- map2_df(.x=trainset_list,.y=testset_list,.f = function(.x,.y) simulator_fragmentation(trainset=.x,testset=.y,time=time,geolocalisation=geolocalisation,outcome=outcome,count=count,factor="month"))

  return(alldata)

}

