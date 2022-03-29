#' Upload the files(variant, age_case) to the ecdc
#'
#' @return Upload the files in data/ecdc
#' @export
#'
#' @examples
upload_ecdc <- function() {
  dir.create("data", showWarnings = FALSE)
  #################################################################################
  # upload data
  #################################################################################
  ########### variant

  download.file(
    url = "https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv",
    destfile = "data/variants_aggregated.csv"
  )
  variants_aggregated <- tibble(read.csv("data/variants_aggregated.csv"))
  save(variants_aggregated, file = "data/variants_aggregated.RData")
  unlink("data/variants_aggregated.csv")
  ########### cases database

  download.file(
    url = "https://opendata.ecdc.europa.eu/covid19/agecasesnational/csv/data.csv",
    destfile = "data/case_aggregated.csv"
  )
  case_aggregated <- tibble(read.csv("data/case_aggregated.csv"))
  save(case_aggregated, file = "data/case_aggregated.RData")
  unlink("data/case_aggregated.csv")



  download.file(
    url = "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv",
    destfile = "data/hospitalisation.csv"
  )
  hospitalisation <- tibble(read.csv("data/hospitalisation.csv"))
  save(hospitalisation, file = "data/hospitalisation.RData")
  unlink("data/hospitalisation.csv")
}
