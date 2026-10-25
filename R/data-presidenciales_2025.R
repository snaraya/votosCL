#' Elecciones presidenciales 2025
#'
#' Base de datos de los resultados electorales de las elecciones 2025.
#' Las elecciones en Chile para el periodo 2026-2030 se realizaron en
#' dos rondas: la primera fue el 16 de noviembre del 2025 en conjunto
#' con las elecciones parlamentarias. La segunda vuelta se realizó
#' el 14 de diciembre del 2025. Estas son las primeras elecciones
#' desde la reintroducción del voto obligatorio en 2022.
#'
#' @format Una base de datos con 566.633 filas y 20 columnas.
#'
#' \describe{
#'   \item{anio}{Año de elección.}
#'   \item{fecha}{Fecha de la vuelta presidencial.}
#'   \item{ronda}{Primera o segunda vuelta presidencial.}
#'   \item{id_region}{Número de región.}
#'   \item{region}{Nombre de la región.}
#'   \item{distrito}{Distrito.}
#'   \item{comuna}{Nombre de la comuna.}
#'   \item{codigo}{Código único territorial de la comuna.}
#'   \item{local}{Local de votación.}
#'   \item{mesa}{Mesa de votación.}
#'   \item{nro_voto}{Número de voto en la papeleta.}
#'   \item{candidato}{Nombre completo del candidato.}
#'   \item{nombres}{Nombre del candidato.}
#'   \item{primer_apellido}{Primer apellido del candidato.}
#'   \item{segundo_apellido}{Segundo apellido del candidato.}
#'   \item{sexo}{Sexo del candidato.}
#'   \item{partido}{Partido político del candidato.}
#'   \item{clasifica}{El candidato clasifica para la segunda vuelta electoral.}
#'   \item{electo}{El candidato que gana la segunda vuelta electoral.}
#'   \item{votos}{Cantidad de votos obtenido por el candidato en la elección.}
#'   \item{porcentaje}{Porcentaje de votación en relación al total comunal.}
#' }
#'
#' @source Servicio Electoral de Chile (SERVEL)
#' @references \url{https://www.servel.cl}
#'
#' @examples
#' head(presidenciales_2025)
#'
"presidenciales_2025"


