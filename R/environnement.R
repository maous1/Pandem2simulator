#' La fonction génére un environnement conda pour l'utilisation des outils nextclade et ncbi datasets
#'
#' @return
#' @export
#' @import reticulate
#' @examplesa

environnement <- function()
{
  condalist = conda_list()
  if(!any(condalist$name == "Pandem2"))
  {
    conda_create(envname = "Pandem2")
    conda_install(envname = "Pandem2",packages = "conda")
    conda_install(envname = "Pandem2",forge = T,packages = "ncbi-datasets-cli"="13.28.1")
    conda_install(envname = "Pandem2",packages = "nextclade",channel="bioconda",version="2.2.0")

  }
  use_condaenv(condaenv = "Pandem2")
}
