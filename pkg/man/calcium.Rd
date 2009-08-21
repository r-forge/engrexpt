\name{calcium}
\Rdversion{1.1}
\alias{calcium}
\docType{data}
\title{
  Calcium levels before and after taking vitamin
}
\description{
  The serum calcium levels in 11 test subjects before taking a
  multi-vitamin containing calcium and three hours after taking the vitamin.
}
\format{
  A data frame with 11 observations on the following 2 variables.
  \describe{
    \item{\code{hrs0}}{serum calcium level (mg/dl) at time zero}
    \item{\code{hrs3}}{serum calcium level (mg/dl) three hours after
      taking the multi-vitamin containing calcium}
  }
}
\details{
    This was a small pilot study to test the effectiveness of the
    multi-vitamin with respect to calcium absorption.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(calcium)
show(pl1 <-
     xyplot(hrs3 ~ hrs0, calcium, type = c("g","p"), aspect = "iso",
            xlab = "Calcium level (mg/dl) before taking multi-vitamin",
            ylab =
            "Calcium level (mg/dl) 3 hours after taking multi-vitamin"))
tmd(pl1)
}
\keyword{datasets}
