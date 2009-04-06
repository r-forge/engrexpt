\name{adhesion}
\Rdversion{1.1}
\alias{adhesion}
\alias{adhesion2}
\docType{data}
\title{
  Adhesive qualities of a lens coating
}
\description{
  These data are from two experiments to examine the effect of pH and
  catalyst on the adhesive qualities of a lens coating.  In the
  \code{adhesion} data ten samples were made and tested at each of three
  pH levels.  In the \code{adhesion2} data five samples were made and
  tested at each of six combinations of pH and catalyst.
}
%\usage{data(adhesion)}
\format{
  \code{adhesion} is a data frame with 30 observations on the following 2 variables.
  \describe{
    \item{\code{adhesion}}{a numeric vector}
    \item{\code{ph}}{a numeric vector at three distinct levels}
  }
  \code{adhesion2} is a data frame with 30 observations on the following 3 variables.
  \describe{
    \item{\code{cat}}{a factor with levels \code{A} \code{B}}
    \item{\code{ph}}{a numeric vector}
    \item{\code{adhesion}}{a numeric vector}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(adhesion)
xtabs(~ ph, adhesion)
xyplot(adhesion ~ ph, adhesion, ylab = "Adhesion of a lens coating",
       xlab = "pH", type = c("g", "p", "a"))
dotplot(as.factor(ph) ~ adhesion, adhesion, ylab = "pH", type =
       c("p","a"), xlab = "Adhesion of a lens coating")
str(adhesion2)
xtabs(~ cat + ph, adhesion2)
dotplot(as.factor(ph) ~ adhesion, adhesion2, groups = cat,
        type = c("p","a"), ylab = "pH",
        auto.key = list(space = "right", lines = TRUE, title = "Catalyst"))
}
\keyword{datasets}
