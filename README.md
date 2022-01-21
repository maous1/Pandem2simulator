# Pandem2simulator
Here is the command to install the package

```
install.packages("devtools")
devtools::install_github("maous1/Pandem2simulator")
```

I propose a script to test the package

```
library(Pandem2simulator)
# Loading the data
data("variants_aggregated")
data("case_aggregated")
# Formated the files
case_aggregated_format <- format_case(case_aggregated)
variants_aggregated_format <- format_variant(variants_aggregated)
# Desaggragated the data
case_desaggregated <- desagregation(case_aggregated_format)
variants_desaggregated <- desagregation(variants_aggregated_format)
# Simulator is applied on belgium cases data from week 2021-19 to 2021-32
resultat_desaggregated = simulator_variant(trainset = variants_desaggregated,testset = case_desaggregated,start = "2021-19",end = "2021-32",country_code = c("BE"))
resultat_aggregated = resultat_desaggregated %>% group_by(country_code,year_week,age_group,variant)%>% summarise(nb = n())

```
