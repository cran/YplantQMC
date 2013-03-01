\name{getR}
\alias{getR}
\title{Get crown radius of a plant}
\description{
Calculates the crown radius of a plant, given the x,y,z coordinates of the leaves. See Details.
Typically not invoked by user, but used by \code{\link{summary.plant3d}} to estimate crown width.
}
\usage{
getR(xyz)
}
\arguments{
  \item{xyz}{A matrix or dataframe of xyz coordinates.}
}
\details{
The crown radius is the distance from the centre of the plant to the furthest leaf in that direction. This distance
is found for four directions (corresponding to the four quadrants). The centre of the plant is found from the average
x,y coordinate (and may thus differ from the 'stem' location). 
}
\value{Returns only the mean crown radius of the four quadrants.
}

\author{
Remko Duursma
}

\seealso{
\code{\link{summary.plant3d}},\code{\link{crownhull}},\code{\link{Silhouette}}
}

\keyword{misc}
