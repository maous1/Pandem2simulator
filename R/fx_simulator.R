#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
fx_simulator <- function(data){

  data("fxvariant")
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
