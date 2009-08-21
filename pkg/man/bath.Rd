\name{bath}
\Rdversion{1.1}
\alias{bath}
\docType{data}
\title{
  Electrical resistance after water bath
}
\description{
  In a production process a rubber material is extruded in a continous
  ribbon (about 2 feet in width).  In one step of the process the
  product passes through a water bath.  In this experiment the time
  spent in the bath and the temperature of the bath were varied to
  determine their effect on the electrical resistance of the final
  product.  
}
\format{
  A data frame with 4 observations on the following 3 variables.
  \describe{
    \item{\code{time}}{time in the bath (coded as levels -1 and 1)}
    \item{\code{temp}}{temperature in the bath (coded as levels -1 and 1)}
    \item{\code{er}}{electrical resistance of the final produce (ohm/\eqn{m^2})}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(bath)
dotplot(ordered(time) ~ er, bath, groups = temp, type = c("p","a"),
        xlab = expression("Electrical resistance (ohm/" * m^2 * ")"),
        ylab = "Time in bath (coded)",
        auto.key = list(columns = 2, lines = TRUE))
summary(fm1 <- lm(er ~ time * temp, bath))
summary(fm2 <- lm(er ~ time + temp, bath))
summary(fm3 <- lm(er ~ temp, bath))
}
\keyword{datasets}
