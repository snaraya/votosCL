#' Resultados de las elecciones municipales en Chile (2004-2024)
#'
#' Base de datos de candidaturas a alcalde para las elecciones
#' municipales chilenas, construida a partir de datos
#' oficiales del Servicio Electoral de Chile (SERVEL).
#'
#' @format Una base de datos con 12.021 filas y 21 columnas.
#' \describe{
#'   \item{anio}{Año de elección.}
#'   \item{id_region}{Número de región.}
#'   \item{region}{Nombre de la región.}
#'   \item{distrito}{Distrito.}
#'   \item{comuna}{Nombre de la comuna.}
#'   \item{cod_comunal}{Código de la comuna.}
#'   \item{id_candidato}{Identificador único de candidato, obtenido desde la base de datos de Trayectorias de SERVEL.}
#'   \item{candidato}{Nombre completo del candidato.}
#'   \item{nombres}{Nombre del candidato.}
#'   \item{primer_apellido}{Primer apellido del candidato.}
#'   \item{segundo_apellido}{Segundo apellido del candidato.}
#'   \item{sexo}{Sexo del candidato.}
#'   \item{rango_etario}{Rango etario del candidato.}
#'   \item{partido}{Partido político del candidato.}
#'   \item{sigla}{Sigla del partido político del candidato.}
#'   \item{lista}{Lista electoral del candidato.}
#'   \item{pacto}{Nombre del pacto o coalición electoral del candidato.}
#'   \item{votos}{Cantidad de votos obtenido por el candidato en la elección.}
#'   \item{electo}{SI si el candidato fue electo como alcalde en esa elección.}
#'   \item{resultado_en_eleccion}{NO ELECTO si el candidato no fue electo en esa elección, ELECTO en el caso contrario.}
#'   \item{porcentaje}{Porcentaje de votación en relación al total comunal.}

#' }
#'
#' @source Servicio Electoral de Chile (SERVEL)
#' @references \url{https://www.servel.cl}
#'
#' @examples
#' head(elecciones_alcalde)
#'
"elecciones_alcalde"
