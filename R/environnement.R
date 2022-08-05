#' La fonction génére un environnement conda pour l'utilisation des outils nextclade et ncbi datasets
#'
#' @param prefix
#'
#' @return
#' @export
#' @import reticulate
#' @examplesa

environnement <- function(prefix)
{
  conda_create(envname = paste0("./",prefix))
  conda_install(envname = paste0("./",prefix),packages = "conda")
  conda_install(envname = paste0("./",prefix),forge = T,packages = "ncbi-datasets-cli",version="13.28.1")
  conda_install(envname = paste0("./",prefix),packages = "nextclade",channel="bioconda",version="2.2.0")
}
