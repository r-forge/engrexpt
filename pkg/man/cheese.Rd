\name{cheese}
\Rdversion{1.1}
\alias{cheese}
\docType{data}
\title{
  Percentage fat in cheese
}
\description{
  A manufacturer of cheese supplies a major pizza chain from three
  different manufacturing locations.  Samples from 6 different batches
  at each of three different plants were assayed for the percentage fat
  content.
}
\format{
  A data frame with 90 observations on the following 3 variables.
  \describe{
    \item{\code{plant}}{the plant where the cheese was manufactured - a
      factor with levels \code{A}, \code{B} and \code{C}}
    \item{\code{batch}}{the batch of cheese - a factor with levels
      \code{a} to \code{f}.  Note that batch \code{a} from plant
      \code{A} is not related to batch \code{a} from plants
      \code{B} or \code{C}}
    \item{\code{fat}}{fat content of the cheese (\%)}
  }
}
\references{
  Peter R. Nelson, Marie Coffin and Karen A. F. Copeland (2003),
  \emph{Introductory Statistics for Engineering Experimentation},
  Elsevier. (Appendix A)
}
\examples{
str(cheese)
bwplot(batch ~ fat|plant, cheese, layout = c(1,3), strip = FALSE,
       strip.left = TRUE,
       xlab = "Percentage fat content of batches from three plants")
dotplot(reorder(reorder(plant:batch, fat), as.numeric(plant)) ~ fat |
        reorder(plant, fat), cheese, strip = FALSE, strip.left = TRUE,
        layout = c(1, 3), scales = list(y = list(relation = "free")),
        aspect = 0.2, type = c("p", "a"), xlab =
"Percentage fat content - lines join mean fat content per batch",
        ylab = "Batch within plant", jitter.y = TRUE)
}
\keyword{datasets}
