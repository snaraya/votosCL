
rm(list = ls())

library(tidyverse)
library(readxl)
library(guaguas)
library(janitor)
library(stringi)
library(labelled)
library(desuctools)

# Paquete guaguas

df_nombres_guaguas <- guaguas |>
  select(nombre, sexo, n) |>
  mutate(across(c(nombre, sexo),
                function(x) stri_trans_general(str_to_lower(x), "latin-ascii"))) |>
  group_by(nombre, sexo) |>
  summarise(n = sum(n))

df_nombres_guaguas <- df_nombres_guaguas |>
  group_by(nombre) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup()

# Sexo

asignar_sexo <- function(data) {

  df_sexo <- data |>
    mutate(primer_nombre = str_extract(nombres, "^\\S+")) |>
    select(nombres, primer_nombre, primer_apellido, segundo_apellido) |>
    filter(!is.na(primer_apellido)) |>
    left_join(df_nombres_guaguas,
              by = c("primer_nombre" = "nombre"),
              multiple = "first") |>
    select(nombres, primer_apellido, segundo_apellido, sexo)

}

procesar_eleccion <- function(data, cols_normalizar, anio_valor, cargo) {

  data |>
    mutate(
      across({{ cols_normalizar }},
             ~ stri_trans_general(str_to_lower(.x), "latin-ascii")),
      anio = anio_valor,
      votos = as.double(votos),
      electo = if_else(cargo == "alcalde", 1, 0)
    ) |>
    group_by(comuna) |>
    mutate(porcentaje = votos / sum(votos, na.rm = TRUE)) |>
    ungroup()
}

# Elecciones 2024 ---------------------------------------------------------

## Candidatos ----

alcaldes_2024 <- read_xlsx("data-raw/2024_10_Alcaldes_Datos_Eleccion.xlsx",
                             skip = 3,
                             sheet = "Votación")

alcaldes_2024 <- alcaldes_2024 |>
  clean_names()

colnames(alcaldes_2024)

alcaldes_2024_clean <- procesar_eleccion(
  data = alcaldes_2024,
  cols_normalizar = region:cargo,
  anio_valor = 2024,
  cargo = cargo
)

## Add sex

df_sexo <- asignar_sexo(alcaldes_2024_clean)

## Join data

alcaldes_2024_completa <- alcaldes_2024_clean |>
  left_join(df_sexo,
            multiple = "first")

usethis::use_data(alcaldes_2024_completa, overwrite = TRUE)

## Resultados ----

alcaldes_2024_total <- read_xlsx("data-raw/2024_10_Alcaldes_Datos_Eleccion.xlsx",
                             skip = 3,
                             sheet = "Votación por comuna")

alcaldes_2024_total <- alcaldes_2024_total |>
  clean_names()

alcaldes_2024_total_clean <- procesar_eleccion(
  data = alcaldes_2024_total,
  cols_normalizar = region:cargo,
  anio_valor = 2024,
  cargo = cargo
)

## Add sex

df_sexo <- asignar_sexo(alcaldes_2024_total_clean)

## Join data

alcaldes_2024_total_completa <- alcaldes_2024_total_clean |>
  left_join(df_sexo,
            multiple = "first")

usethis::use_data(alcaldes_2024_total_completa, overwrite = TRUE)







# Elecciones 2021 ---------------------------------------------------------

elecciones_2021 <- read_xlsx("data-raw/2021_05_Alcaldes_Datos_Eleccion.xlsx", skip = 4)

elecciones_2021 <- elecciones_2021 |>
  clean_names() |>
  select(-nro_voto)

colnames(elecciones_2021)

elecciones_2021_clean <- elecciones_2021 |>
  mutate(across(region:cargo,
                function(x) stri_trans_general(str_to_lower(x), "latin-ascii")),
         anio = 2021,
         votos = as.double(votos))

usethis::use_data(elecciones_2021_clean, overwrite = TRUE)

elecciones_2021_resumen <- elecciones_2021_clean |>
  select(anio, nro_region, region, comuna, lista, pacto, partido, nombres, primer_apellido, segundo_apellido,
         cargo, votos) |>
  group_by(nro_region, region, comuna, lista, pacto, partido, nombres, primer_apellido, segundo_apellido,
           cargo) |>
  summarise(votos = sum(votos)) |>
  ungroup()

usethis::use_data(elecciones_2021_resumen, overwrite = TRUE)

# Elecciones 2016 ---------------------------------------------------------

alcaldes_2016 <- read_xlsx("data/2016_10_Alcaldes_DatosEleccion.xlsx",
                           skip = 4)

colnames(alcaldes_2016)

alcaldes_2016 <- alcaldes_2016 |>
  clean_names() |>
  select(-nro_voto)



# Elecciones 2004 y 2008 --------------------------------------------------

alcaldes_04_08 <- read_xlsx("data/resultados_elecciones_alcaldes_ce_2004_2016.xlsx",
                           #skip = 4
                           )

colnames(alcaldes_04_08)

alcaldes_04_08 <- alcaldes_04_08 |>
  clean_names(ano_de_eleccion, id_region, region, circunscripcion_senatorial, distrito,
              comuna, circunscripcion_electoral, candidato_a, nombre_circunscripcion_electoral,
              apellido_paterno, apellido_materno, sexo, electo_a, partido, sigla_partido) |>
  select(-nro_voto)

alcaldes_04_08 |>
  count(Cargo)

# Elecciones 2004 ---------------------------------------------------------


