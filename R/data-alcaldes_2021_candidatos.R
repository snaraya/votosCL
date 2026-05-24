#' Candidaturas a alcalde en elecciones municipales 2021 en Chile
#'
#' Base de datos de candidaturas a alcalde para las elecciones
#' municipales chilenas de 2021, construida a partir de datos
#' oficiales del Servicio Electoral de Chile (SERVEL).
#'
#' @format A data frame with 345 rows and 10 variables:
#' \describe{
#'   \item{nro_region}{Número de región}
#'   \item{region}{Nombre de la región}
#'   \item{circunscripcion_senatorial}{Circunscripción senatorial}
#'   \item{distrito}{Distrito}
#'   \item{comuna}{Nombre de la comuna}
#'   \item{circunscripcion_electoral}{Circunscripción electoral}
#'   \item{local}{Local de votación}
#'   \item{mesa}{Número de mesa}
#'   \item{lista}{Lista electoral}
#'   \item{pacto}{Pacto o coalición electoral}
#'   \item{partido}{Partido político}
#'   \item{nombres}{Nombres de candidato}
#'   \item{primer_apellido}{Primer apellido del candidato}
#'   \item{segundo_apellido}{Segundo apellido del candidato}
#'   \item{votos}{Cantidad de votos}
#'   \item{cargo}{Cargo al que postula}
#'   \item{anio}{Año de elección}
#'   \item{electo}{Fue o no electo (1 = Si)}
#'   \item{porcentaje}{Porcentaje de votación}
#'   \item{sexo}{Sexo del candidato}
#' }
#'
#' @source Servicio Electoral de Chile (SERVEL)
#' @references \url{https://www.servel.cl}
#'
#' @examples
#' head(alcaldes_2021_candidatos)
#'
"alcaldes_2021_candidatos"
