#' La fonction génére un environnement conda pour l'utilisation des outils nextclade et ncbi datasets
#'
#' @return
#' @export
#'
#' @examples
environnement <- function()
{
  condalist = conda_list()
  if(!any(condalist$name == "Pandem2"))
  {
    conda_create(envname = "Pandem2")
    conda_install(envname = "Pandem2",packages = "conda")
    conda_install(envname = "Pandem2",forge = T,packages = "ncbi-datasets-cli")
    conda_install(envname = "Pandem2",packages = "nextclade",channel="bioconda")

  }
  use_condaenv(condaenv = "Pandem2")
}
