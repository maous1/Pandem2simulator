#' Upload the files(variant, age_case, vaccination and hospitalisation) to the ecdc
#'
#' @return Upload the files in data/ecdc
#' @export
#'
#' @examples
Download_ECDC <- function() {
  dir.create("data", showWarnings = FALSE)
  dir.create("data/ecdc", showWarnings = FALSE)
  dir.create("results", showWarnings = FALSE)
  dir.create("results/prediction", showWarnings = FALSE)
  #################################################################################
  # Download data
  #################################################################################
  ########### variant

  download.file(url = "https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv",
                destfile = "data/ecdc/variants.csv")

  ########### cases database

  download.file(url = "https://opendata.ecdc.europa.eu/covid19/agecasesnational/csv/data.csv",
                destfile = "data/ecdc/age_cases.csv")

  ########### Vaccination

  download.file(url = "https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv",
                destfile = "data/ecdc/vaccination.csv")

  ########### hospitalisation database

  download.file(url = "https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv",
                destfile = "data/ecdc/hospitalisation.csv")
}
