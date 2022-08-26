#' Merge the metadata and genomic_data by the col_merge.
#'
#' Merge the metadata and genomic_data by the col_merge.
#' Metadata and genomic_data must have the same name of the column that will be used for merge.
#'
#' @param metadata the dataset metadata containing the column col_merge
#' @param genomic_data The dataset containing the substitutions, deletions and missing data. The form of this dataset is based on the output of nextclade and differents mutation.
#' @param col_merge The name of the column that will be used to merge the data
#' @param count the name of the column used to desaggregate the metadata
#' @param time the name of the column where the dates are found format = "%Y-%m-%d"
#' @param mutation
#'
#' @return The function adds according to col_merge the columns of mutations coming from genomic_data in metadata
#' @export add_genomic_data
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import tibble
#' @importFrom splitstackshape expandRows
#'
add_genomic_data <- function(metadata, genomic_data, col_merge, count, time, mutation = T) {
  if (!any(names(metadata) %in% col_merge)) {
    if (!any(names(genomic_data) %in% col_merge)) {
      stop("wrong col_merge in genomic_data and metadata")
    }
    stop("wrong col_merge in  metadata")
  }
  if (!any(names(genomic_data) %in% col_merge)) {
    stop("wrong col_merge in genomic_data")
  }
  if (!any(names(metadata) %in% count)) {
    stop("wrong count in metadata")
  }
  genomic_data <- genomic_data %>% rownames_to_column(var = "cas")
  genomic_data_cas <- genomic_data %>%select(cas,!!time,!!col_merge)
  genomic_data_cas$nb <- rep(1,length(genomic_data_cas$cas))
  genomic_data_cas <-genomic_data_cas%>% rename(!!count := nb)

  case_variants_aggregated_cas <- simulator(bymonth = F,
                                            trainset = genomic_data_cas,
                                            testset = metadata,
                                            time = time,
                                            geolocalisation = col_merge,
                                            outcome = "cas",
                                            count = count,
                                            factor = 2000)
  genomic_data <- genomic_data %>% select(-c(year_week,time,variant))

  genomic_data_with_metadata <- case_variants_aggregated_cas %>% left_join(genomic_data,by = "cas")%>% select(-cas)

  # if(mutation ==T) {
  #   genomic_data_with_metadata_long <- genomic_data_with_metadata %>%
  #     pivot_longer(cols = -c(names(metadata),!!count),names_to = "mutation",values_to = "presence") %>%
  #     group_by_at(names(.)[names(.) != count])%>%summarise(!!count := sum(across(all_of(count))))
  #   return(genomic_data_with_metadata_long)
  # }
  #
  return(genomic_data_with_metadata)
}
