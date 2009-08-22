\name{webtraff}
\Rdversion{1.1}
\alias{webtraff}
\docType{data}
\title{
  Web site traffic during a marketing campaign
}
\description{
  Web site traffic during a marketing campaign
}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{weeks}}{weeks into the marketing campaign - an ordered
      factor with levels \code{1} to \code{5}}
    \item{\code{traffic}}{web site traffic (1000's of hits per day)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(webtraff)
dotplot(weeks ~ traffic, webtraff, type = c("p","a"),
        xlab = "Web traffic (1000's of hits per day)",
        ylab = "Weeks into the marketing campaign")
}
\keyword{datasets}
