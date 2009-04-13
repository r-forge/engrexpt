\name{caliper}
\Rdversion{1.1}
\alias{caliper}
\docType{data}
\title{
  Diameters of rods measured by two calipers
}
\description{
  Diameters of 14 metal rods measured once each with each of two calipers.
}
\format{
  A data frame with 14 observations on the following 3 variables.
  \describe{
    \item{\code{part}}{indicator of which part is measured - a factor}
    \item{\code{calipera}}{measurement with caliper type A}
    \item{\code{caliperb}}{measurement with caliper type B}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(caliper)
show(pl1 <-
     xyplot(caliperb ~ calipera, caliper,
            type = c("g","p"), aspect = "iso",
            xlab = "Measurement with caliper A",
            ylab = "Measurement with caliper B"))
tmd(pl1)
}
\keyword{datasets}
