#' Códigos comunales de Chile
#'
#' Base de datos que contiene el código único territorial
#' y el nombre de la comuna. El nombre de la comuna está
#' estandarizado para ser utilizado con las bases de datos
#' del Servel.
#'
#' ¿Qué quiere decir estandarizado?
#' En este caso, se eliminaron las tildes de las comunas.
#'
#' @format Una base de datos con 346 filas y 2 columnas.
#' \describe{
#'   \item{codigo}{Códigos únicos territoriales (2018).}
#'   \item{comuna}{Nombre de la comuna.}
#' }
#'
#' @source Sistema Nacional de Información Municipal (SINIM)
#' @references \url{https://www.sinim.gov.cl}
#'
#' @examples
#' head(codigos_comunales)
#'
"codigos_comunales"
