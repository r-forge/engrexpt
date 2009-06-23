\name{uvcoatin}
\Rdversion{1.1}
\alias{uvcoatin}
\docType{data}
\title{
  Comparison of eyeglass ultra-violet coatings
}
\description{
  Comparison of eyeglass ultra-violet coatings
}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{a}}{a numeric vector of differences in haze values for the commerical coating}
    \item{\code{b}}{a numeric vector of differences in haze values for the commerical coating}
    \item{\code{diff}}{a numeric vector of the differences in the two above vectors}
  }
}
\details{A field test of a new coating was conducted with 10 voluenteers who wore glasses on a regular
basis.  Each subject had one lens coated with a comerical product and one lens coated with a test product.
The coatings were applied to the lens in a random fashion such that some subjects had the comerical coating
on their right lens and some on thier left lens.  The haze of each lens was measured at the begining and the end of the
study with the difference in haze being the measure of interest.}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(uvcoatin)
}
\keyword{datasets}
