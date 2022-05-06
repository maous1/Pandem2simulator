#' Data set from the ECDC age group
#'
#' Data set from the ECDC. The dataset contains the data of Belgium between the 20th week and the 32nd week of 2021. The dataset has been formatted to be inserted in the simulator function.
#'
#' @docType data
#' @keywords datasets
#' @format A data frame :
#' \describe{
#'   \item{country_code}{country_code of countries}
#'   \item{year_week}{The year and the week}
#'   \item{time}{The day of the year in  = "%Y-%m-%d"}
#'   \item{age_group}{Age group}
#'   \item{new_cases}{The number of cases}
#' }
#' @source \url{https://opendata.ecdc.europa.eu/covid19/agecasesnational/csv/data.csv}
#' @usage data(case_aggregated_formatted_BE)
"case_aggregated_formatted_BE"


#' Data set from the ECDC variant
#'
#' Data set from the ECDC. The dataset contains the data of Belgium between the 20th week and the 32nd week of 2021. The dataset has been formatted to be inserted in the simulator function.
#' @docType data
#' @keywords datasets
#'
#' @format A data frame :
#' \describe{
#'   \item{country_code}{country_code of countries}
#'   \item{year_week}{The year and the week}
#'   \item{time}{The day of the year in  = "%Y-%m-%d"}
#'   \item{variant}{The sars cov 2 variant}
#'   \item{new_cases}{The number of cases}
#' }
#' @source \url{https://opendata.ecdc.europa.eu/covid19/virusvariant/csv/data.csv}
#' @usage data(variants_aggregated_formatted_BE)
"variants_aggregated_formatted_BE"

#' Data set from the ECDC hospitalisation
#'
#' Data set from the ECDC. The dataset contains the data of Belgium between the 20th week and the 32nd week of 2021. The dataset has been formatted to be inserted in the simulator function.
#' @docType data
#' @keywords datasets
#' @format A data frame :
#' \describe{
#'   \item{country_code}{country_code of countries}
#'   \item{year_week}{The year and the week}
#'   \item{time}{The day of the year in  = "%Y-%m-%d"}
#'   \item{new_cases}{The number of cases}
#' }
#' @source \url{https://opendata.ecdc.europa.eu/covid19/hospitalicuadmissionrates/csv/data.csv}
#' @usage data(hospitalisation_formatted_BE)
"hospitalisation_formatted_BE"

#' Dataset containing information from variant and mutation
#'
#' Dataset containing information from variant and mutation
#' @docType data
#' @keywords datasets
#' @format A data frame :
#' \describe{
#'   \item{variant}{The sars cov 2 variant}
#'   \item{substitutions}{Numbers of positions of substitutions with mutation}
#'   \item{deletions}{Numbers of deletion positions}
#'   \item{insertions}{Numbers of the positions of the insertions}
#'   \item{missing}{The numbers of the positions that have not been sequenced}
#' }
#' @usage data(genomic_data)
"genomic_data"
