#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
fx_simulator <- function(data){

  packagedata <- data("fxvariant")
  data$code = "BE"
  packagedata$code = "BE"

  retourne <- simulator(trainset = packagedata,
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
