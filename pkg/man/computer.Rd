\name{computer}
\Rdversion{1.1}
\alias{computer}
\docType{data}
\title{
  Repair time of computers
}
\description{
  Data from a study of the repair time for different brands of computers
  in different configurations.
}
\format{
  A data frame with 36 observations on the following 3 variables.
  \describe{
    \item{\code{brand}}{a factor with levels \code{A} \code{B} \code{C}}
    \item{\code{type}}{an ordered factor with levels \code{Inexpensive
	home} < \code{Expensive home} < \code{Business}}
    \item{\code{time}}{time to repair computer (minutes)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(computer)
dotplot(type ~ time, computer, groups = brand,
        auto.key = list(columns = 3, lines = TRUE),
        type = c("p","a"), jitter.y = TRUE,
        xlab = "Time to repair computer (minutes)")
summary(fm1 <- lm(time ~ brand * type, computer))
summary(fm2 <- lm(time ~ brand + type, computer))
summary(fm3 <- lm(time ~ type, computer))
}
\keyword{datasets}
