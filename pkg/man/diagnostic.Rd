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
\details{
  Diagnostic kits are used in clinics or laboratories to test samples
  (e.g., blood) for some condition (i.e., illness).  One kit can often
  test many samples.  In this case one kit can test at least 16 samples.
  This study was to look the consistency between kits used to test the
  same set of samples.  The samples would have been divided such that both
  kits could be used to test the sample and then the results compared.
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
