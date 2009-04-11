\name{diagnostic}
\Rdversion{1.1}
\alias{diagnostic}
\docType{data}
\title{
 Consistency of diagnostic kits
}
\description{
 Consistency of diagnostic kits
}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{kit1}}{measurement using kit 1}
    \item{\code{kit2}}{measurement using kit 2}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(diagnostic)
show(plt1 <-
     xyplot(kit2 ~ kit1, diagnostic,
            aspect = "iso", type = c("g", "p"),
            xlab = "Measurement using kit 1",
            ylab = "Measurement using kit 2"))
tmd(plt1)
show(plt2 <-
     xyplot(kit2 ~ kit1, diagnostic, aspect = "iso",
            xlab = "Measurement using kit 1 - logarithmic axis",
            ylab = "Measurement using kit 2 - logarithmic axis",
            scales = list(log = 2)))
tmd(plt2)
}
\keyword{datasets}
