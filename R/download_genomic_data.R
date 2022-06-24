library(dplyr)
library(tidyr)
library(lubridate)
library(seqinr)

#Pour tous les input, chemin relatif par rapport à celui du script
#ncbitools.dir : suppose que datasets et dataformat soit dans le meme dossier
#nextclade.data.dir : dossier nextclade avec tous les fichiers de paramètres pour Sars-Cov-2
#nextclade.exfile : fichier executable nextclade

download_genomic_data <- function(ncbitools.dir, nextclade.data.dir, nextclade.exfile) {

  #Download SARS-CoV-2 genomes from NCBI (Europe)
  ncbitools.dir <- '../Téléchargements/'
  myarg <- 'download virus genome taxon sars-cov-2 --host human --geo-location europe --complete-only --exclude-cds --exclude-protein' #--released-since 05/01/2022
  system2(command=paste0(ncbitools.dir, 'datasets'), args=myarg)
  system2("unzip", args = c("-o", "ncbi_dataset.zip"), stdout = TRUE)
  myarg <- 'tsv virus-genome --inputfile ncbi_dataset/data/data_report.jsonl --fields accession,biosample-acc,virus-pangolin,geo-location,isolate-collection-date,nucleotide-completeness'
  system2(command= paste0(ncbitools.dir, 'dataformat'), args=myarg, stdout="ncbi_dataset/data/data_report_formated-lineage.tsv")

  #Filters
  ncbi_metadata <- read.table("ncbi_dataset/data/data_report_formated-lineage.tsv", sep = '\t', header = TRUE)
  #Change date format of Collection_Date variable
  date_format <- function(x){
    return(strftime(x, format = "%Y-%V"))}
  ncbi_metadata$Isolate.Collection.date <- as.character(lapply(as.Date(ncbi_metadata$Isolate.Collection.date),date_format))
  ncbi_metadata.final <- ncbi_metadata %>% mutate(Isolate.Collection.date = replace(Isolate.Collection.date, Isolate.Collection.date == "2021-53", "2020-53")) %>% drop_na(Isolate.Collection.date)

  #1. ecdc variant


  #2. Filtering the number of occurrences (10 max) by variants per week
  ncbi_metadata.final <- ncbi_metadata.final %>% group_by(Isolate.Collection.date, Virus.Pangolin.Classification) %>% slice (1:10)

  #3. Select sequences in NCBI file
  NCBI.genomes <- read.fasta(file = "ncbi_dataset/data/genomic.fna", seqtype = "DNA", as.string = TRUE, set.attributes = FALSE)
  NCBI.genomes.filter <- NCBI.genomes[c(which(names(NCBI.genomes) %in% ncbi_metadata.final$Accession))]
  write.fasta(sequences = NCBI.genomes.filter, names = names(NCBI.genomes.filter), file.out = "ncbi_dataset/data/genomic_filter.fna")

  #NextClade - Parameters and launch
  dir.create("Nextclade_results", showWarnings = FALSE)
  output.file.nextclade <- c('Nextclade_results/res_Nextclade.tsv')
  output.dir.nextclade <- c('Nextclade_results')
  nextclade.data.dir <- c('data/sars-cov-2/') #a supprimer
  myarg <- paste0('--input-fasta ncbi_dataset/data/genomic_filter.fna --input-dataset ', nextclade.data.dir,' --input-root-seq ', paste0(nextclade.data.dir, 'reference.fasta'), ' --input-tree ', paste0(nextclade.data.dir, 'tree.json'), ' --input-qc-config ', paste0(nextclade.data.dir, 'qc.json'), ' --input-gene-map ', paste0(nextclade.data.dir, 'genemap.gff'), ' --output-tsv ', output.file.nextclade, ' --output-dir ', output.dir.nextclade)
  nextclade.exfile <- '../Téléchargements/nextclade-Linux-x86_64' #a supprimer
  system2(command=nextclade.exfile, args=myarg)

  #Load NextClade file and NCBI metadata
  nextclade.res <- read.table(output.file.nextclade, sep='\t', header=TRUE)
  genomic.data <- merge(x=nextclade.res, y=ncbi_metadata.final, by.x='seqName', by.y='Accession', sort = TRUE)
  genomic.data <- genomic.data %>% select(Isolate.Collection.date, Nextclade_pango, substitutions, deletions, insertions, missing) %>% rename(collection_date=Isolate.Collection.date) %>% rename(variant=Nextclade_pango)

  #Save output files
  return(genomic.data.final)
}

download_genomic_data ('../Téléchargements/', 'data/sars-cov-2/','../Téléchargements/nextclade-Linux-x86_64')
