\name{battery}
\Rdversion{1.1}
\alias{battery}
\docType{data}
\title{
  Battery lifetime in laptop computers
}
\description{
  Data from  a study on the effect of different processors on the
  battery lifetime in laptop computers.
}
\format{
  A data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{type}}{processor type - a factor with levels \code{A},
      \code{B} and \code{C}}
    \item{\code{lifetime}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(battery)
densityplot(~ lifetime, battery, groups = type,
            xlab = "Battery lifetime (hours)",
            auto.key = list(columns = 3, lines = TRUE))
densityplot(~ lifetime, battery, groups = type,
            scales = list(x = list(log = 2)),
            xlab = "Battery lifetime (hours) - logarithmic scale",
            auto.key = list(columns = 3, lines = TRUE))
dotplot(reorder(as.factor(type), lifetime) ~ lifetime, battery,
        jitter.y = TRUE,
        xlab = "Battery lifetime (hours)",
        ylab = "Type", type = c("p", "a"))
dotplot(reorder(as.factor(type), lifetime) ~ lifetime, battery,
        jitter.y = TRUE, scales = list(x = list(log = 2)),
        xlab = "Battery lifetime (hours) - logarithmic scale",
        ylab = "Type", type = c("p", "a"))
}
\keyword{datasets}
