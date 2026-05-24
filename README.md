
<!-- README.md is generated from README.Rmd. Please edit that file -->

# votosCL <img src="man/figures/my_sticker.png" align="right" width = "120px"/>

<!-- badges: start -->

<!-- badges: end -->

Descripción:

¿Qué es lo nuevo?

- Sexo en las últimas elecciones.
- Estandarización de los nombres.
- Recopilación de todas la información desde las elecciones en 1989.

## Instalación

El paquete votosCL puedes instalarlo a través de GitHub:

``` r
# install.packages("devtools")
devtools::install_github("snaraya/votosCL")
```

## Datos

El paquete se compone de múltiples bases de datos que contienen los
resultados electorales del Servicio Electoral de Chile (SERVEL). Las
bases de datos se componen por:

### Bases de datos completas “en bruto”

Descripción tabla

### Base de datos resultados por candidato

Descripción tabla

### Base de datos histórico por tipo de elección

Descripción tabla

## Ejemplos

A continuación se muestran algunos ejemplos de exploración de estos
datos:

Resultados electorales por género:

``` r
library(votosCL)

# Limpieza de datos hasta gráfico de barras stacked
```

Resultados electorales por partido:

Se seleccionan los partidos históricos más estables.

``` r
# Limpieza de datos hasta gráfico
```

## Fuente de los datos

Los datos fueron obtenidos a través de la página del Servicio Electoral
de Chile.

## Paquetes similares:

- Paquetes de otros países.
