\name{curl}
\Rdversion{1.1}
\alias{curl}
\docType{data}
\title{
  Curl of transparencies
}
\description{
  The current formulation of a coating on transparencies used in ink-jet
  printers causes them to curl.  These data are from experiments
  conducted on a new formulation of the coating.  
}
\format{
  A data frame with 12 observations on the following 3 variables.
  \describe{
    \item{\code{catalyst}}{amount of catalyst}
    \item{\code{compa}}{amount of component a (the second component)}
    \item{\code{curl}}{a measure of the curl}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(curl)
xtabs(~ catalyst + compa, curl)   ## display the experimental design
xyplot(curl ~ compa | ordered(catalyst), curl,
       type = c("g","p","r"), layout = c(3,1),
       xlab = "Amount of component `a' - panels are amount of catalyst",
       ylab = "Amount of curl")
}
\keyword{datasets}
