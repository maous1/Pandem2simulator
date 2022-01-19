# Pandem2
Here is the command to install the package

```
install.packages("devtools")
devtools::install_github("maous1/Pandem2")
```

I propose a script to test the package

```
library(Pandem)
#loading the data
data("variants_aggregated")
data("case_aggregated")
#put the files in the right format
case_aggregated_format <- case_format(case_aggregated)
variants_aggregated_format <- variant_format(variants_aggregated)
#desaggragated the data
case_desaggragated <- desagregation(case_aggregated_format)
variants_desaggragated <- desagregation(variants_aggregated_format)
#simulator test on belgium in week 31-2021
list_country_code = c("BE")
resultat = simulator_variant(trainset = variants_desaggragated,testset = case_desaggragated,start = "2021-30",end = "2021-32",list_country_code = list_country_code)
```
