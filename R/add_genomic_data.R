#' Title
#'
#' @param metadata
#' @param genomic_data
#' @param col_merge
#' @param count
#'
#' @return
#' @export
#'
#' @examples
add_genomic_data <- function(metadata, genomic_data, col_merge, count) {
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


  names(metadata)[names(metadata) %in% col_merge] <- "variant_metadata"
  names(genomic_data)[names(genomic_data) %in% col_merge] <- "variant"

  metadata <- expandRows(metadata, count = count, drop = T)

  # Create table with genomic data associated to sample metadata
  genomic_data_with_metadata <- tibble()

  sample_genomic <- function(tmp_variant,tmp_sample_metadata)
  {
    sample_genomic_data <- tmp_variant[sample(
      x = nrow(tmp_variant),
      size = nrow(tmp_sample_metadata),
      replace = TRUE
    ), ]
    tmp <- cbind(tmp_sample_metadata,sample_genomic_data)
  }

  genomic_data <- genomic_data %>% split(.$variant)
  metadata_split = metadata %>% split(.$variant_metadata)
  common_variant <- intersect(names(genomic_data), names(metadata_split))

  genomic_data <- genomic_data[is.element(names(genomic_data), common_variant)]
  metadata_NSQ <- do.call(rbind, metadata_split[!is.element(names(metadata_split), common_variant)])

  metadata_SQ <- metadata_split[is.element(names(metadata_split), common_variant)]

  genomic_data_with_metadata <- map2_df(.x = genomic_data, .y = metadata_SQ, .f = function(.x, .y) sample_genomic(tmp_variant = .x, tmp_sample_metadata = .y))



  genomic_data_with_metadata <- union_all(genomic_data_with_metadata, metadata_NSQ)
  # Remove original country column, collection date and ecdc variant (redundant)
  genomic_data_with_metadata <- genomic_data_with_metadata %>%
    select(-country, -variant, -collection_date,-lineage,-pangolin_version,-clade,-biosample,-SRA_accession,-accession,scorpio_call)


  names(genomic_data_with_metadata)[names(genomic_data_with_metadata) %in% "variant_metadata"] <- col_merge
  return(genomic_data_with_metadata)
}
