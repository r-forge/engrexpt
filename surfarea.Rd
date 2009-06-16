\name{surfarea}
\alias{surfarea}
\docType{data}
\title{Surface area of silica}
\description{
  Measurements of the surface area, in \eqn{m^2/g}, of samples of
  silica, a chemical product with many applications, such as a filler in
  rubber products.  The surface area is a key property of the product.
}
\format{
  A data frame with 32 observations on the following variable.
  \describe{
    \item{\code{area}}{a numeric vector}
  }
}
%\details{}
%\source{}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(surfarea)
with(surfarea, summary(area))
qqmath(~ area, surfarea, aspect = 1, type = c("g", "p"),
       xlab = "Standard normal quantiles",
       ylab = expression("Surface area " * (m^2/g) *
           " of batches of silica"))
print(qqmath(~ area, surfarea, aspect = 1, type = c("g", "p"),
             xlab = "Standard normal quantiles",
             ylab = expression("Surface area " * (m^2/g))),
      pos = c(0,0,0.33,1), more = TRUE)
print(densityplot(~ area, surfarea,
                  xlab = expression("Surface area "*(m^2/g)*
                      " of batches of silica")),
      pos = c(0.3,0,1,1))
}
\keyword{datasets}
