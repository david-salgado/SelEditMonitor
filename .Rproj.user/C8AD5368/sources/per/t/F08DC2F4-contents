#' Inversa de Logit(x) para cada uno de los elementos x del vector de entrada.
#'
#' \code{invLTA} Calcula la inversa de Logit(x) para cada uno de los elementos x del vector.
#'
#' @param p \code{Vector} de tipo \code{numeric} sobre el que se calculará la función.
#'
#' @param adjust \code{Vector} de tipo \code{numeric} de longitud 1. Por defecto toma el valor
#' \code{10^-15}.
#'
#' @return \code{Vector} de tipo \code{numeric} de longitud igual al vector de entrada \code{p} con
#' el valor de la función inversa del Logit para cada uno de los elementos del vector.
#'
#' @examples
#' dontrun{
#'
#'}
#'
#' @export
invLTA <- function (p, adjust = 0.000000000000001, unit = "degrees"){
  
  if (length(adjust) != 1) stop("[invLTA] El parametro adjust debe tener longitud 1.")
  
  if (!unit %in% c("degrees", "radians")) stop("[invLTA] Los valores validos para el parametro unit son: degrees, radians.")
  
  if (adjust <= 0 | adjust > 0.5) stop("[invLTA] El parametro adjust debe ser un numero positivo y no mayor que 0.5")
  
  p <- as.numeric(p)
  a <- 1 - 2 * adjust
  x <- 0.5 /(a * (1 + exp(-p)))
  x[x < 0] <- 0
  x[is.nan(x)] <- 0.5 / a
  x <- sqrt(x)
  output <- numeric(length(x))
  output[is.na(p)] <- NA
  output[!is.na(p) & x < 1] <- acos(x[!is.na(x) & x < 1])
  if (unit == "degrees")
    output <- output * 180/pi
  return(output)
}
