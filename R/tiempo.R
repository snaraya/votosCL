#' Que hora es
#'
#' @param language Idioma, "es" o "en"
#'
#' @returns Cadena de caracteres.
#' @export
#'
#' @examples
#' what_time()
#' what_time("en")
what_time <- function(language = "es") {
  if(language == "fr"){
    stop("Not french please")
  }

  rlang::arg_match0(language, c("es", "en"))

  time <- format(Sys.time(), "%H:%M")

  exclamation <- praise::praise("${Exclamation}")

  switch(
    language,
    es = sprintf("%s! Ahora son las %s!", exclamation, time),
    en = sprintf("%s! It is %s now!", exclamation, time)
  )
}





