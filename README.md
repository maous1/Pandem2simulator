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

case_aggregated_format <- format_case(case_aggregated,datedepart = "2021-01-04",datefin = "2021-10-03")
variants_aggregated_format <- format_variant(variants_aggregated,datedepart = "2021-01-04",datefin = "2021-10-03")

# Simulator is applied on belgium cases data from 2021-01-04 to 2021-10-03
variants_aggregated_formatted_BE <-   variants_aggregated_formatted %>% 
  filter(country_code == "BE")
case_aggregated_BE <-   case_aggregated %>% 
  filter(country_code == "BE")
resultat_desaggregated = simulator_variant(trainset = variants_aggregated_formatted_BE,
  testset = case_aggregated_BE,
  geolocalisation = "country_code",
  outcome = "variant",
  count = 'new_cases', 
  time= "time")

```
