\name{lightresponse}
\alias{lightresponse}
\title{
Non-rectangular hyperbola
}
\description{
A simple light response function that predicts leaf photosynthesis from absorbed PAR.
}
\usage{
lightresponse(PAR, Amax, phi, theta, Rd, ...)
}
\arguments{
  \item{PAR}{Photosynthetically active radiation (mu mol m-2 s-1)}
  \item{Amax}{Maximum assimilation rate (the asymptote) (mu mol m-2 s-1)}
  \item{phi}{Quantum yield (slope at PAR = 0) (mol mol-1)}
  \item{theta}{Shape of light response curve (0 = rectangular hyperbola, 1 = 'Blackman' response).}
  \item{Rd}{Dark respiration (**positive** value). }
  \item{\dots}{Further arguments are ignored.}
}
% \details{
% }
\value{
Returns a dataframe with :
\describe{
\item{A}{Net assimilation rate (mu mol m-2 s-1)}
}


}
\author{
Remko Duursma
}

\seealso{\code{\link{setPhy}}}
% \examples{}
\keyword{misc}

