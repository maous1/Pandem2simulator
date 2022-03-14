#' Title
#'
#' @param trainset
#' @param testset
#' @param time
#' @param geolocalisation
#' @param outcome
#' @param count
#' @param factor
#'
#' @return
#' @export
#'
#' @examples
simulator_running <- function(trainset, testset,time ,geolocalisation, outcome, count= NULL,factor){


  set.seed(2)

  names(trainset)[names(trainset) %in% geolocalisation]<-"geolocalisation"
  names(trainset)[names(trainset) %in% time]<-"time"
  names(testset)[names(testset) %in% geolocalisation]<-"geolocalisation"
  names(testset)[names(testset) %in% time]<-"time"

  #trainset <- trainset %>% mutate(new_cases = as.integer(round(new_cases/factor))) %>% filter(new_cases >0)
  trainset <- trainset%>%group_by(geolocalisation,time)%>%
    mutate(somme = sum(new_cases)) %>%
    mutate(new_cases = as.integer(round(new_cases/somme*factor)))%>%
    filter(new_cases >0)

  trainset <- expandRows(trainset,count=count,drop=T)
  testset <- expandRows(testset,count=count,drop=T)

  #trainset <- trainset %>% group_by(geolocalisation,time)%>%slice_sample(n = factor)

  print(length(trainset$time))





  stat_allgeolocalisation <- data.frame()
  # For each geolocalisation
  unique_geo <- unique(trainset$geolocalisation)
  for (location in unique_geo) {

    # Data train
    train_geolocalisation <- trainset %>% filter(geolocalisation == location)
    train_geolocalisation <- train_geolocalisation %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_itter = time_num + rnorm(length(time_num),0,3))

    V1 <- c("time_itter")
    train_geolocalisation_V1<- train_geolocalisation[V1]

    classificateur = train_geolocalisation[outcome]


    # Data test
    test_geolocalisation <- testset %>% filter(geolocalisation == location)
    test_geolocalisation <- test_geolocalisation %>%
      mutate(time_num = as.numeric(as.Date(time))) %>%
      mutate(time_itter = time_num + rnorm(length(time_num),0,3))
    test_geolocalisation_V1 <- test_geolocalisation[V1]
    # Prediction with KNN model
    pr <- knn(train = data.frame(train_geolocalisation_V1),
              test = data.frame(test_geolocalisation_V1),
              cl = classificateur[[1]],
              k = 1)

    # Create variant variable
    test_geolocalisation[outcome] <- as.character(pr)

    # Concatenate prediction file for all countries

    test_geolocalisation <- test_geolocalisation %>%
      select(-c(time_itter, time_num))%>% group_by_all()%>% summarise(nb=n())
    stat_allgeolocalisation <- union_all(stat_allgeolocalisation, test_geolocalisation)

  }
  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "geolocalisation"]<-geolocalisation
  names(stat_allgeolocalisation)[names(stat_allgeolocalisation) %in% "time"]<-time
  return(stat_allgeolocalisation)
}
