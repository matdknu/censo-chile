library(dplyr)
library(readxl)
library(janitor)
library(censo2017)
library(sjmisc)
library(tidyverse)
library(chilemapas)

################################################################################
################################# CENS0 2017 ###################################
################################################################################

variables <- censo_tabla("variables")
variables_codificacion <- censo_tabla("variables_codificacion")

variables

variables %>% filter(variable == "p16")
variables %>% filter(variable == "p16a")


indigena_total <- tbl(censo_conectar(), "zonas") %>% 
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>% 
  #filter(region == "08") %>% 
  select(comuna, geocodigo, zonaloc_ref_id, region) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"), zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id, hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id, indigena = p16), by = "hogar_ref_id") %>%
  collect()


indigena_total <- indigena_total %>% 
  group_by(region, indigena) %>%
  summarise(cuenta = n()) %>%
  group_by(region) %>%
  mutate(proporcion = cuenta / sum(cuenta))


# 1️⃣ Calcular proporción indígena por región
indigena_region <- indigena_total %>%
  filter(indigena == 1) %>%
  group_by(region) %>%
  summarise(proporcion = mean(proporcion, na.rm = TRUE), .groups = "drop") %>%
  mutate(codigo_region = str_pad(region, width = 2, pad = "0"))

# 2️⃣ Unir con mapa de regiones (sf)
mapa_datos <- mapa_regiones %>%
  left_join(indigena_region, by = "codigo_region")

# 3️⃣ Definir colores rústicos
colores_rusticos <- c("#DCA761", "#C6C16D", "#8B9C94", "#628CA5", "#b8c5cf")

# 4️⃣ Graficar
g2 <-ggplot(mapa_datos) +
  geom_sf(aes(fill = proporcion), color = "black", size = 0.2) +
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -17), expand = FALSE) +
  scale_fill_gradientn(
    colours = colores_rusticos,
    limits = c(0, 0.5),  # Escala fija de 0 a 0.5
    name = "Proporción"
  ) +
  theme_minimal() +
  labs(
    title = "% de Habitantes Indígenas",
    subtitle = "Chile continental",
    caption = "Fuente: Censo 2017"
  )

g2

################################################################################
################################# CENS0 2024 ###################################
################################################################################

# --- Librerías ---
library(readxl)
library(janitor)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(chilemapas)

# --- 1. Cargar y procesar población indígena ---
pob_indigena <- read_xlsx(
  "data/raw/P2-Pueblos-indigenas.xlsx",
  skip = 3, sheet = 2
) %>%
  clean_names() %>%
  drop_na(region) %>%
  filter(region != "País") %>%
  rename(
    pob_identificada = poblacion_que_es_o_se_considera_perteneciente_a_un_pueblo_indigena_u_originario
  ) %>%
  mutate(codigo_region = as.numeric(codigo_region))

# --- 2. Cargar y procesar población censada ---
pob_censada <- read_excel(
  "data/raw/D1_Poblacion-censada-por-sexo-y-edad-en-grupos-quinquenales.xlsx",
  skip = 3, sheet = 5
) %>%
  clean_names() %>%
  select(1:8) %>%
  filter(region != "País",
         grupos_de_edad == "Total Comuna") %>%
  group_by(codigo_region) %>%
  summarise(suma = sum(poblacion_censada, na.rm = TRUE), .groups = "drop") %>%
  mutate(codigo_region = as.numeric(codigo_region))

# --- 3. Calcular proporción de población indígena ---
poblacion_indigena <- pob_indigena %>%
  right_join(pob_censada, by = "codigo_region") %>%
  mutate(
    ratio = pob_identificada / suma,
    codigo_region = str_pad(codigo_region, width = 2, pad = "0")
  ) %>%
  select(region, codigo_region, pob_identificada, suma, ratio)


mapa_regiones <- chilemapas::mapa_comunas %>%
  mutate(codigo_region = substr(codigo_comuna, 1, 2)) %>%
  group_by(codigo_region) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()


# --- 4. Mapa de regiones ---
mapa_regiones <- mapa_regiones %>%
  select(codigo_region, geometry) %>%
  st_as_sf()

# Unir mapa con datos
pob_mapa <- mapa_regiones %>%
  left_join(poblacion_indigena, by = "codigo_region")

# --- 5. Graficar con colores rústicos ---
colores_rusticos <- c("#DCA761", "#C6C16D", "#8B9C94", "#628CA5", "#b8c5cf")

g1 <- ggplot(pob_mapa) +
  geom_sf(aes(fill = ratio), color = "black", size = 0.2) +
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -17), expand = FALSE) +
  scale_fill_gradientn(
    colours = colores_rusticos,
    limits = c(0, 0.5),  # Escala fija de 0 a 0.5
    name = "Proporción"
  ) +
  theme_minimal() +
  labs(
    title = "% Indígena por región",
    caption = "Fuente: CENSO 2024"
  )

g1

ggsave(g1, )

# POB MAPA - MAPA DATOS

pob_mapa$anio <- 2024
mapa_datos$anio <- 2017

pob_mapa <- pob_mapa |> rename(ratio_2024 = ratio)
mapa_datos <- mapa_datos |> rename(ratio_2017 = proporcion)



