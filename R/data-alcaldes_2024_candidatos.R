#' Candidaturas a alcalde en elecciones municipales 2024 en Chile
#'
#' Base de datos de candidaturas a alcalde para las elecciones
#' municipales chilenas de 2024, construida a partir de datos
#' oficiales del Servicio Electoral de Chile (SERVEL).
#'
#' @format Una base de datos con 2268 filas y 16 columnas.
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
#' head(alcaldes_2024_candidatos)
#'
"alcaldes_2024_candidatos"
