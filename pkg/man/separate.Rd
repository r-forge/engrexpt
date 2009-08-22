\name{separate}
\Rdversion{1.1}
\alias{separate}
\docType{data}
\title{
  Electrical resistance of battery separators
}
\description{
  Electrical resistance of battery separators
}
\format{
  A data frame with 24 observations on the following 4 variables.
  \describe{
    \item{\code{silica}}{a factor with levels \code{High} \code{Low}}
    \item{\code{time}}{a factor with levels \code{High} \code{Low}}
    \item{\code{temp}}{a factor with levels \code{High} \code{Low }}
    \item{\code{y}}{a numeric vector}
  }
}
\details{
  This experiment was run to study the effect of three process factors
  on the electrical resistance of a battery separator (extruded from a
  rubber, oil, and silica mix).  Each of the three factors, occur at two
  levels, designated "High" and "Low".
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(separate)
}
\keyword{datasets}
