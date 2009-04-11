\name{fish}
\Rdversion{1.1}
\alias{fish}
\alias{fish2}
\docType{data}
\title{
  Toxin concentrations in fish by site
}
\description{
  Toxin concentrations in fish by site
}
\format{
  \code{fish} is a data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{site}}{a factor with levels \code{A}, \code{B}, \code{C}
      and \code{D}}
    \item{\code{toxin}}{a numeric vector}
  }
  \code{fish2} is a data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{site}}{a factor with levels \code{E}, \code{F}, \code{G}
      and \code{H}}
    \item{\code{toxin}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(fish)
dotplot(reorder(site, toxin) ~ toxin, fish, jitter.y = TRUE,
        aspect = 0.3, xlab = "Amount of toxin (mg/kg wet weight)")
str(fish2)
dotplot(reorder(site, toxin) ~ toxin, fish2, jitter.y = TRUE,
        aspect = 0.3, xlab = "Amount of toxin (mg/kg wet weight)")
}
\keyword{datasets}
