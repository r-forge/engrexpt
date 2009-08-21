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
  \code{fish2} is a data frame with 16 observations on the following 2
  variables.
  \describe{
    \item{\code{site}}{a factor with levels \code{E}, \code{F}, \code{G}
      and \code{H}}
    \item{\code{toxin}}{a numeric vector}
  }
}
\details{
  Prior to beginning mining operations a new precious metals mine had to
  run an extensive environmental study of the surrounding area,
  particularly down stream from their construction site.  Thus, a
  baseline of data was collected over many years so that any
  environmental impact of the facility could be quantified.  One step in
  the study was to collect fish along a stream and measure the amount of
  various toxins found in the fish.
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
