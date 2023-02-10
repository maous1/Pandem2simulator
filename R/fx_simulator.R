#' Title
#'
#' @param data
#' @param startdate
#'
#' @return
#' @export
#' @import dplyr
#' @examples
fx_simulator <- function(data,startdate){

  data("fxvariant")
  startdate = as.Date(startdate, format = "%Y-%m-%d" )
  newdate = data.frame((startdate+(-2:45)*7),unique(fxvariant$time))

  colnames(newdate) <- c("new","time")
  fxvariant <- fxvariant %>% left_join(newdate)

  fxvariant$time<-fxvariant$new
  fxvariant$new <- NULL

  data$code = "BE"
  fxvariant$code = "BE"
  fxvariant$cases <- fxvariant$new_cases
  fxvariant$new_cases <- NULL
  retourne <- simulator(trainset = fxvariant,
                        testset = data,
                        var_names_time = time,
                        var_names_geolocalisation = code,
                        var_names_outcome = variant,
                        var_names_count = cases,
                        factor = 500,
                        bymonth = F
  )
  retourne$code = NULL
  return(retourne)

}
