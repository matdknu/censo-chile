library(dplyr)
library(readxl)
library(janitor)

# cargar datos ----
pob_sexo_edad <- read_xlsx("datos/originales/D1_Poblacion-censada-por-sexo-y-edad-en-grupos-quinquenales.xlsx",
                           skip = 3,
                           sheet = 5) |> 
  clean_names()

# sexo y edad ----
## población por sexo ----
pob_sexo_comuna <- pob_sexo_edad |> 
  filter(codigo_region != 0) |> 
  filter(grupos_de_edad == "Total Comuna")

pob_sexo_comuna_2 <- pob_sexo_comuna |> 
  select(codigo_region, region, codigo_comuna, comuna, 
         pob_total = poblacion_censada, pob_hombres = hombres, pob_mujeres = mujeres, pob_sexo_razon = razon_hombre_mujer)

# guardar
readr::write_csv2(pob_sexo_comuna_2, "datos/censo_2024_pob_sexo.csv")


## población por edad ----
pob_edad <- pob_sexo_edad |> 
  filter(codigo_region != 0) |> 
  filter(grupos_de_edad != "Total Comuna")

pob_edad_2 <- pob_edad |> 
  select(-hombres, -mujeres, -razon_hombre_mujer) |> 
  tidyr::pivot_wider(names_from = grupos_de_edad, values_from = poblacion_censada,
                     names_prefix = "pob_") |> 
  clean_names() |> 
  mutate(pob_14 = pob_0_a_4 + pob_5_a_9 + pob_10_a_14,
         pob_29 = pob_15_a_19 + pob_20_a_24 + pob_25_a_29,
         pov_85 = pob_70_a_74 + pob_75_a_79 + pob_80_a_84 + pob_85_o_mas) |> 
  select(codigo_region, region, codigo_comuna, comuna, 
         starts_with("pob"))

readr::write_csv2(pob_edad_2, "datos/censo_2024_pob_edad.csv")
