#' Title
#'
#' @param metadata
#' @param genomic_data
#' @param col_merge
#'
#' @return
#' @export
#'
#' @examples
merge_genomic <- function(metadata,genomic_data,col_merge){

  names(metadata)[names(metadata) %in% col_merge]<-"variant_metadata"
  names(genomic_data)[names(genomic_data) %in% col_merge]<-"variant"

  # Create table with genomic data associated to sample metadata
  genomic_data_with_metadata <- tibble()

  for(var in unique(genomic_data$variant)){

    tmp_variant <- genomic_data %>%
      filter(variant == var)
    tmp_sample_metadata <- metadata %>%
      filter(variant_metadata == var)
    metadata <- metadata %>%
      filter(variant_metadata != var)
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


  names(metadata)[names(metadata) %in% "variant_metadata"]<-paste0(col_merge,"_metadata")
  names(genomic_data)[names(genomic_data) %in% "variant"]<-col_merge
  return(genomic_data_with_metadata)
}
