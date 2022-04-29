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
data("variants_aggregated_formatted_BE")
data("case_aggregated_formatted_BE")
# Formated the files


# Simulator is applied on belgium cases data from week 2021-19 to 2021-32
resultat = simulator(trainset = variants_aggregated_formatted_BE,
  testset = case_aggregated_BE,
  geolocalisation = "country_code",
  outcome = "variant",
  count = 'new_cases', 
  time= "time",
  factor = 500)

```
