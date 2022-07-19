#' Title
#'
#' @param ecdcdata
#' @param colname
#'
#' @return
#' @export
#'
#' @import tidyr
#' @import dplyr
#' @import lubridate
#' @import seqinr
#'
#' @examples
download_genomic_data <- function(ecdcdata, colname) {
  ###Filters : 1. list of ecdc variant
  list.ecdc.variants <- ecdcdata%>% ungroup() %>% select(!!colname) %>% distinct %>% filter(!grepl("SGTF|XF|XD|UNK|NSQ|Other|/|\\+", !!as.name(colname)))
  ecdc.variants <- unlist(list.ecdc.variants, use.names = FALSE)
  ##Download SARS-CoV-2 genomes from NCBI (Europe) only for list of ecdc variant
  myarg <- paste0('download virus genome taxon sars-cov-2 --host human --geo-location europe --complete-only --exclude-cds --exclude-protein --lineage ', ecdc.variants)
  for(i in 1:length(myarg)){
    system2(command="./genomic_data_pandem2/bin/datasets", args=paste0(myarg[i], ' --filename ncbi_dataset', i, '.zip'))
    system2("unzip", args = c("-o", paste0('ncbi_dataset', i ,'.zip'), "-d", paste0('ncbi_dataset', i)), stdout = TRUE)
    if (file.exists(paste0('ncbi_dataset', i, '/ncbi_dataset/data/data_report.jsonl'))) {
      myarg2 <- paste0('tsv virus-genome --inputfile ncbi_dataset', i,'/ncbi_dataset/data/data_report.jsonl --fields accession,biosample-acc,virus-pangolin,geo-location,isolate-collection-date,nucleotide-completeness')
      system2(command="./genomic_data_pandem2/bin/dataformat", args=myarg2, stdout=paste0('data_report_formated-lineage', i,'.tsv'))
    }
  }
  ##Rbind all ncbi metadata files for each variants
  files <- list.files(pattern = '\\.tsv')
  ncbi_metadata <- readr::read_tsv(files)
  colnames(ncbi_metadata) <- c("Accession", "BioSample.accession", "Virus.Pangolin.Classification", "Geographic.location", "Isolate.Collection.date","Nucleotide.completeness")

  ###Filters : 2. The number of occurrences (10 max) by variants per week
  ncbi_metadata.final <- ncbi_metadata %>% group_by(Isolate.Collection.date, Virus.Pangolin.Classification) %>% dplyr::slice (1:10)
  ##Rbind all ncbi genomes files for each variants
  genomes <- list.files(pattern = '\\.fna', recursive=TRUE) %>% lapply(readDNAStringSet)
  NCBI.genomes <- do.call(c, genomes)
  ###Filters : 3. Select sequences in NCBI file
  names(NCBI.genomes) <- gsub( " .*$", "", names(NCBI.genomes))
  NCBI.genomes.filter <- NCBI.genomes[ncbi_metadata.final$Accession]
  writeXStringSet(NCBI.genomes.filter, "genomic_filter.fna")

  ###NextClade - Parameters and launch
  system2(command="./genomic_data_pandem2/bin/nextclade", args='dataset get --name sars-cov-2 --output-dir data/sars-cov-2')
  system2(command="./genomic_data_pandem2/bin/nextclade", args='run -D data/sars-cov-2 --input-root-seq data/sars-cov-2/reference.fasta --input-tree data/sars-cov-2/tree.json --input-qc-config data/sars-cov-2/qc.json --input-gene-map data/sars-cov-2/genemap.gff --output-all Nextclade_results genomic_filter.fna')
  ##Load NextClade file and NCBI metadata
  nextclade.res <- read.table('Nextclade_results/nextclade.tsv', sep='\t', header=TRUE)
  genomic.data <- merge(x=nextclade.res, y=ncbi_metadata.final, by.x='seqName', by.y='Accession', sort = TRUE)
  genomic.data <- genomic.data %>% select(Isolate.Collection.date, Nextclade_pango, substitutions, deletions, insertions, missing) %>% dplyr::rename(collection_date=Isolate.Collection.date) %>% dplyr::rename(variant=Nextclade_pango)

  #Cleanning and return output file
  sapply(paste0("ncbi_dataset", 1:length(list.ecdc.variants), ".zip"), unlink)
  unlink("ncbi_dataset*", recursive = TRUE)
  unlink("data_report_formated-lineage*", recursive = TRUE)
  return(genomic.data)

}
