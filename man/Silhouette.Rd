\name{Silhouette}
\alias{Silhouette}

\title{
Calculates the area of the 2D convex hull of a projected plant
}
\description{
to be added
}
\usage{
Silhouette(obj, azimuth = NA, altitude = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Either a \code{plant3d} object, or a \code{projectedplant3d} object.}
  \item{azimuth,altitude}{Viewing azimuth and altitude (ignored if a projected plant is given as input).}
}

\author{
Remko Duursma
}
\note{
Not usually called by the user. Use instead, \code{\link{STARbar}} and \code{\link{projectplant}}.
For the latter, see the option \code{silhouette=TRUE}.
}


\seealso{\code{\link{summary.plant3d}},\code{\link{STARbar}},\code{\link{projectplant}}
}
\examples{

# Silhouette returns the area of the 2D convex hull (H), and the coordinates of the 2D hull (xyz):
Silhouette(sugarmaple, altitude=0, azimuth=45)

}



\keyword{misc}
