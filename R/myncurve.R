#' Title Normal Curve Maker
#'
#' @param mu Mean value of distribution
#' @param sigma Standard deviation of distribution
#' @param a limit of X
#'
#' @return A normal curve with a shaded area, as well as what the area (probability) under the curve is
#' @export
#'
#' @examples myncurve(2, 5, 3)
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(-1000, a, length(10000))
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(-1000, xcurve, a), c(0, ycurve, 0), col = "Blue")
  area = pnorm(a, mu, sigma)
  arear = round(area, 4)
  list(Area = area)
}
myncurve(2, 5, 3)
