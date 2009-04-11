\name{applicat}
\alias{applicat}
\docType{data}
\title{Application of powder coating}
\description{ 
  As described in Nelson, Coffin and Copeland (2003), \dQuote{The
    application of powder coating (a type of paint used on appliances
    and, in limited settings, cars) is done by spraying the material
    through a \sQuote{gun} that has an electrical charge on it. There
    are three factors to consider in setting up the application booth:
    the gun distance from the target item, the charge and the pressure
    (flow rate) of the material through the gun. This data set contains
    data from 18 runs.}
}
\format{
  A data frame with 18 observations on the following 4 variables.
  \describe{
    \item{\code{distance}}{distance of the gun from the target (inches) - an
      ordered factor with levels \code{11} < \code{13} < \code{15}}
    \item{\code{charge}}{charge used (kvolts) - an ordered factor with
      levels \code{35} < \code{45} < \code{55}}
    \item{\code{flowrate}}{flow rate in gm/min. - an ordered factor with
      levels \code{94} < \code{124}}
    \item{\code{gloss}}{a measure of the resulting gloss}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(applicat)
dotplot(distance ~ gloss|flowrate, applicat, groups = charge,
        type = c("p","a"), layout = c(1,2),
        ylab = "Gun distance from target (in.)",
        strip = FALSE, strip.left = TRUE,
        auto.key = list(columns = 3, lines = TRUE), xlab =
"Gloss for two flow rates (gm/min) and three charges (kvolts)")
dotplot(charge ~ gloss|flowrate, applicat, groups = distance, 
        type = c("p","a"), layout = c(1,2),
        ylab = "Charge (kvolts)",
        strip = FALSE, strip.left = TRUE,
        auto.key = list(columns = 3, lines = TRUE), xlab =
"Gloss for two flow rates (gm/min) and three distances from target (in)")
}
\keyword{datasets}
