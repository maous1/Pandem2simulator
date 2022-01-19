# Pandem2simulator
Here is the command to install the package

```
install.packages("devtools")
devtools::install_github("maous1/Pandem2simulator")
```

I propose a script to test the package

```
library(Pandem)
#loading the data
data("variants_aggregated")
data("case_aggregated")
#put the files in the right format
case_aggregated_format <- format_case(case_aggregated)
variants_aggregated_format <- format_variant(variants_aggregated)
#desaggragated the data
case_desaggragated <- desagregation(case_aggregated_format)
variants_desaggragated <- desagregation(variants_aggregated_format)
#simulator test on belgium in week 31-2021
resultat_desaggregated = simulator_variant(trainset = variants_desaggragated,testset = case_desaggragated,start = "2021-30",end = "2021-32",country_code = c("BE"))
resultat_aggregated = resultat_desaggregated %>% group_by(country_code,year_week,age_group,variant)%>% summarise(nb = n())

```
