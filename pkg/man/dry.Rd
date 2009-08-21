\name{dry}
\Rdversion{1.1}
\alias{dry}
\docType{data}
\title{
  Moisture content after drying
}
\description{
  Moisture content after drying
}
\format{
  A data frame with 16 observations on the following 3 variables.
  \describe{
    \item{\code{rate}}{a factor with levels \code{SLOW} and \code{FAST}}
    \item{\code{type}}{type of dryer - a factor with levels
      \code{BATCH} and \code{TUNNEL}}
    \item{\code{response}}{Moisture content of final product (\%)}
  }
}
\details{
  A production process used two types of dryers.  The product could pass
  through the tunnel dryer as a web of material or the product could be
  coiled on larger rollers and then placed into a dryer.  The question
  was how to best set each dryer to obtain a moisture content of 63(\%).
}
\examples{
str(dry)
}
\keyword{datasets}
