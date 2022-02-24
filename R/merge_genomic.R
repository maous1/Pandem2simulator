#' Title
#'
#' @param metadata
#' @param genomic_data
#'
#' @return
#' @export
#'
#' @examples
merge_genomic <- function(metadata,genomic_data){

  # Create table with genomic data associated to sample metadata
  genomic_data_with_metadata <- tibble()

  for(var in unique(genomic_data$variant)){

    tmp_variant <- genomic_data %>%
      filter(variant == var)
    tmp_sample_metadata <- metadata %>%
      filter(variant == var) %>%
      rename(variant_ecdc = variant)
    metadata <- metadata %>%
      filter(variant != var)
    sample_genomic_data <- tmp_variant[sample(x = nrow(tmp_variant),
                                              size =  nrow(tmp_sample_metadata),
                                              replace = TRUE),]
    tmp <- cbind(sample_genomic_data, tmp_sample_metadata)
    genomic_data_with_metadata <- rbind(genomic_data_with_metadata, tmp)
  }

  genomic_data_with_metadata = union_all(genomic_data_with_metadata, metadata)
  # Remove original country column, collection date and ecdc variant (redundant)
  genomic_data_with_metadata <- genomic_data_with_metadata %>%
    select(-country, - variant_ecdc, -collection_date)

  return(genomic_data_with_metadata)
}
