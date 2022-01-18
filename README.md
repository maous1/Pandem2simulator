# Pandem2
Here is the command to install the package

```
install.packages("devtools")
devtools::install_github("maous1/Pandem2")
```

I propose a script to test the package

```
library(Pandem)
Download_ECDC()
case = Table_case()
alldata = Table_data()
hospitalisation = Table_hospitalisation()
list_country_code <- case %>% select(country_code) %>% distinct %>% filter(country_code != "BG" & country_code == "BE")
resultat = Prediction(data = case,start = "2021-30",end = "2021-32",list_country_code = list_country_code)
```
