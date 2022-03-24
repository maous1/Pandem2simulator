#' desaggragated a data with a column "new_cases". Each line for N patient to one line per patient
#'
#' @param aggregate data aggregated. The data needs to have a "new_cases" column
#'
#' @return
#' @export
#'
#' @examples
desagregation <- function(aggregate) {
  desagregation <- expandRows(aggregate, count = "new_cases", drop = T)

  return(desagregation)
}
