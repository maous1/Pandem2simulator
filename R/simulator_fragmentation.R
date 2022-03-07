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
simulator_fragmentation <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL,factor){
  set.seed(2)

  names(trainset)[names(trainset) %in% geolocalisation]<-"geolocalisation"
  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% geolocalisation]<-"geolocalisation"
  names(testset)[names(testset) %in% time]<-"time"

  unique_geo <- unique(trainset$geolocalisation)

  if(length(count)>0){
    trainset <- expandRows(trainset,count=count,drop=T)
    testset <- expandRows(testset,count=count,drop=T)
  }


  stat_allgeolocalisation <- data.frame()

  date = (variants_aggregated_formatted_BE %>% group_by(time)%>%
            summarise(case_week= sum(new_cases))%>%
            mutate(cum= cumsum(case_week))%>%mutate(index = floor(cum/factor))%>%group_by(index)%>%slice(1))$time


  for (i in 1:(length(date)-1)) {
    trainset_frag <- trainset %>% filter(as.Date(time)>=date[i] & as.Date(time)<=date[i+1])
    testset_frag <- testset %>% filter(as.Date(time)>=date[i] & as.Date(time)<=date[i+1])

    # For each geolocalisation
    for (location in unique_geo) {

      # Data train
      train_geolocalisation <- trainset_frag %>% filter(geolocalisation == location)
      train_geolocalisation <- train_geolocalisation %>%
        mutate(time_num = as.numeric(as.Date(time))) %>%
        mutate(time_unif =round(time_num + runif(1,-5,5),digits = 4))

      V1 <- c("time_unif")
      train_geolocalisation_V1<- train_geolocalisation[V1]

      class = train_geolocalisation[outcome]


      # Data test
      test_geolocalisation <- testset_frag %>% filter(geolocalisation == location)
      test_geolocalisation <- test_geolocalisation %>%
        mutate(time_num = as.numeric(as.Date(time))) %>%
        mutate(time_unif =round(time_num + runif(1,-5,5),digits = 4))
      test_geolocalisation_V1 <- test_geolocalisation[V1]
      # Prediction with KNN model
      pr <- knn(train = data.frame(train_geolocalisation_V1),
                test = data.frame(test_geolocalisation_V1),
                cl = class[[1]],
                k = 1)

      # Create variant variable
      test_geolocalisation[outcome] <- as.character(pr)

      # Concatenate prediction file for all countries

      test_geolocalisation <- test_geolocalisation %>%
        select(-c(time_unif, time_num)) %>% group_by_all()%>% summarise(nb=n())
      stat_allgeolocalisation <- union_all(stat_allgeolocalisation, test_geolocalisation)

    }
  }

  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "geolocalisation"]<-geolocalisation
  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "time"]<-time
  return(stat_allgeolocalisation)
}

