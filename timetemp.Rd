\name{timetemp}
\alias{timetemp}
\docType{data}
\title{Time to nominal temperature}
\description{
  The time for a painted panel to reach a nominal temperature of -10 C
  versus the temperature in a freezer to prepare the panel.  Two types
  of panels were used, original equipment manufacturer (OEM) panels and
  repair panels.  The repair panels have an extra coat of paint.
}
\format{
  A data frame with 24 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{time, in minutes, for the panel to reach the
      nominal temperature of -10 C}
    \item{\code{temp}}{temperature in the freezer from which the panel
      is extracted}
    \item{\code{type}}{a factor with levels \code{Repair} \code{OEM}}
  }
}
%\details{}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(timetemp)
xyplot(time ~ temp, timetemp, groups = type,
       type = c("g","p","r"), aspect = 'xy',
       ylab = "Time to reach -10C (min.)",
       xlab = "Temperature in freezer (degrees C)",
       auto.key = list(columns = 2, lines = TRUE))
}
\keyword{datasets}
