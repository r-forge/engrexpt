\name{sarea}
\alias{sarea}
\docType{data}
\title{Surface area of silica}
\description{
  Measurements of the surface area (\eqn{m^2/g}) of three batches of
  silica as performed by four different lab technicians.
}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{batch}}{a factor with levels \code{a}, \code{b} and \code{c}}
    \item{\code{tech}}{a factor with levels \code{A}, \code{B}, \code{C}
      and \code{D}}
    \item{\code{sarea}}{the surface area in \eqn{m^2/g} as measured by
      the lab technician on a sample from the batch}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(sarea)
xtabs(sarea ~ batch + tech, sarea)
dotplot(reorder(tech, sarea) ~ sarea, sarea,
        groups = batch, type = c("p","a"), aspect = "xy",
        ylab = "Technician",
        xlab = expression("Measured surface area " * (m^2/g) *
                          " of silica batches"),
        auto.key = list(columns = 3, lines = TRUE)
)
}
\keyword{datasets}
