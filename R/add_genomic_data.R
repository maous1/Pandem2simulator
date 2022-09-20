#' Merge the metadata and genomic_data by the col_merge.
#'
#' Merge the metadata and genomic_data by the col_merge.
#' Metadata and genomic_data must have the same name of the column that will be used for merge.
#'
#' @param metadata the dataset metadata containing the column col_merge
#' @param genomic_data The dataset containing the substitutions, deletions and missing data. The form of this dataset is based on the output of nextclade and differents mutation.
#' @param col_merge The name of the column that will be used to merge the data
#' @param var_names_count the name of the column used to desaggregate the metadata
#' @param var_names_time the name of the column where the dates are found format = "%Y-%m-%d"
#' @param mutation
#'
#' @return The function adds according to col_merge the columns of mutations coming from genomic_data in metadata
#' @export add_genomic_data
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @import rlang
#' @importFrom splitstackshape expandRows
#'
add_genomic_data <- function(metadata, genomic_data, var_names_merge, var_names_count, var_names_time) {
  enquo_merge <- enquo(var_names_merge)
  enquo_count <- enquo(var_names_count)
  enquo_time <- enquo(var_names_time)
  if (!any(names(metadata) %in% as_label(enquo_merge))) {
    if (!any(names(genomic_data) %in% as_label(enquo_merge))) {
      stop("wrong var_names_merge in genomic_data and metadata")
    }
    stop("wrong var_names_merge in  metadata")
  }
  if (!any(names(genomic_data) %in% as_label(enquo_merge))) {
    stop("wrong var_names_merge in genomic_data")
  }
  if (!any(names(metadata) %in% as_label(enquo_count))) {
    stop("wrong count in metadata")
  }
  genomic_data <- genomic_data %>% rownames_to_column(var = "cas")
  genomic_data_cas <- genomic_data %>% select(cas, !!enquo_time, !!enquo_merge)
  genomic_data_cas$nb <- rep(1, length(genomic_data_cas$cas))
  genomic_data_cas <- genomic_data_cas %>% rename({{ enquo_count }} := nb)
  case_variants_aggregated_cas <- simulator(
    bymonth = F,
    trainset = genomic_data_cas,
    testset = metadata,
    var_names_time = {{ var_names_time }},
    var_names_geolocalisation = {{ var_names_merge }},
    var_names_outcome = cas,
    var_names_count = {{ var_names_count }},
    factor = 2000
  )
  genomic_data <- genomic_data %>% select(-c(year_week, {{ enquo_time }}, variant))

  genomic_data_with_metadata <- case_variants_aggregated_cas %>%
    left_join(genomic_data, by = "cas") %>%
    select(-cas)

  return(genomic_data_with_metadata)
}
