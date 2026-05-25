#' Candidaturas a alcalde en elecciones municipales 2021 en Chile
#'
#' Base de datos de candidaturas a alcalde para las elecciones
#' municipales chilenas de 2021, construida a partir de datos
#' oficiales del Servicio Electoral de Chile (SERVEL). Originalmente
#' las elecciones municipales estaban previstas para el domingo 25 de octubre
#' de 2020, pero fueron postergadas debido a la pandemia de coronavirus,
#' quedando para el 04 de abril de 2021.
#'
#' @format Una base de datos con 2142 filas y 16 columnas.
#' \describe{
#'   \item{anio}{Año de elección.}
#'   \item{id_region}{Número de región.}
#'   \item{region}{Nombre de la región.}
#'   \item{distrito}{Distrito.}
#'   \item{comuna}{Nombre de la comuna.}
#'   \item{candidato}{Nombre completo del candidato.}
#'   \item{nombres}{Nombre del candidato.}
#'   \item{primer_apellido}{Primer apellido del candidato.}
#'   \item{segundo_apellido}{Segundo apellido del candidato.}
#'   \item{sexo}{Sexo del candidato.}
#'   \item{partido}{Partido político del candidato.}
#'   \item{lista}{Lista electoral del candidato.}
#'   \item{pacto}{Nombre del pacto o coalición electoral del candidato.}
#'   \item{electo}{Dicotómica. Candidato fue (1) o no (0) electo en la elección.}
#'   \item{votos}{Cantidad de votos obtenido por el candidato en la elección.}
#'   \item{porcentaje}{Porcentaje de votación en relación al total comunal.}

#' }
#'
#' @source Servicio Electoral de Chile (SERVEL)
#' @references \url{https://www.servel.cl}
#'
#' @examples
#' head(alcaldes_2021_candidatos)
#'
"alcaldes_2021_candidatos"
