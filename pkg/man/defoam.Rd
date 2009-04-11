\name{defoam}
\Rdversion{1.1}
\alias{defoam}
\docType{data}
\title{
  Height of solution with defoamer
}
\description{
  The height of a solution containing a \dQuote{defoamer} in a 50-ml
  graduated cylinder after being heated to a particular temperature.
}
\format{
  A data frame with 27 observations on the following 4 variables.
  \describe{
    \item{\code{conc}}{concentration of defoamer - a factor with levels
      \code{L} < \code{M} < \code{H}}
    \item{\code{pH}}{pH of defoamer - a factor with levels
      \code{L} < \code{M} < \code{H}}
    \item{\code{temp}}{temperature of defoamer - a factor with levels
      \code{L} < \code{M} < \code{H}}
    \item{\code{height}}{height of solution in the graduated cylinder}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(defoam)
dotplot(pH ~ height|conc, defoam, groups = temp, aspect = 0.3,
        xlab = "Height of solution", type = c("p","a"),
        auto.key = list(space = "right", lines = TRUE,
                        title = "Temperature"),
        strip = FALSE, strip.left = TRUE,
        ylab = "pH within concentration")
summary(fm1 <- lm(height ~ (conc + pH + temp)^2, defoam))
summary(fm2 <- lm(height ~ (pH + temp)^3, defoam))
}
\keyword{datasets}
