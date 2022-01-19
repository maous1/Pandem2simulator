#' desaggragated a data
#'
#' @param aggregate
#'
#' @return
#' @export
#'
#' @examples
desagregation <- function(aggregate) {

  desagregation <- expandRows(aggregate,count='new_cases',drop=T)

  return(desagregation)
}
