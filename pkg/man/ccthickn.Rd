\name{ccthickn}
\Rdversion{1.1}
\alias{ccthickn}
\docType{data}
\title{
  Clear coat thickness
}
\description{
  Measurements of film build from 40 test panels sprayed in a pilot
  plant spray booth to study paint equipment set-up.  Target film build
  was 65 microns.
}
\format{
  A data frame with 40 observations on the following variable.
  \describe{
    \item{\code{thickness}}{clear coat thickness (microns)}
  }
}
\details{
  Cars are coated (i.e., painted) in layers.  The \dQuote{film build}
  (i.e. thickness) of each layer of the coating has an effect on the
  properties of the paint (performance and appearance) so it is
  important to maintain the correct film builds. The final layer of
  paint on a car is called the clear coat; the film build of this
  layer was to be 65 microns.
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(ccthickn)
densityplot(~ thickness, ccthickn, xlab = "Clear coat thickness (microns)")
qqmath(~ thickness, ccthickn, aspect = 1,
       ylab = "Clear coat thickness (microns)",
       xlab = "Standard normal quantiles",
       type = c("g","p"),
       panel=function(...)
   {
       panel.qqmathline(..., alpha = 0.5, lty = 2)
       panel.qqmath(...)
   })
}
\keyword{datasets}
