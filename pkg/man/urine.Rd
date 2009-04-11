\name{urine}
\Rdversion{1.1}
\alias{urine}
\docType{data}
\title{
  Mercury level in employee urine samples
}
\description{
  Mercury level in employee urine samples
}
\format{
  A data frame with 12 observations on the following 5 variables.
  \describe{
    \item{\code{month}}{an ordered factor with levels from
      \code{January} to \code{December}}
    \item{\code{person1}}{mercury concentration in person 1's urine}
    \item{\code{person2}}{mercury concentration in person 2's urine}
    \item{\code{person3}}{mercury concentration in person 3's urine}
    \item{\code{person4}}{mercury concentration in person 4's urine}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(urine)
dotplot(month ~ person1 + person2 + person3 + person4, urine,
        outer = FALSE, type = c("p","l"),
        auto.key = list(columns = 2, lines = TRUE, points = FALSE),
        xlab = "Mercury level in urine")
}
\keyword{datasets}
