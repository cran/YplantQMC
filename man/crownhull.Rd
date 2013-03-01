\name{crownhull}
\alias{crownhull}
\title{ Calculates and plots the convex hull around the plant crown}
\description{

	This function finds the convex hull (and its surface area and volume)
	spanning the leaves of the 3D plant, using all coordinates of the leaf edges. 
	The result is smallest set of x, y, z points that defines the
	convex hull, that is, the polyhedral surface that contains all
	other points, and is convex.
	
	The implementation is a wrapper for the 'convhulln' function in the from package 'geometry'. 
}
\usage{
crownhull(xyz, plotit = TRUE, alpha = 0.8)
}
\arguments{
  \item{xyz}{An object of class 'plant3d', or a matrix with three columns (xyz coordinates).}
  \item{plotit}{Logical. If FALSE, returns only volume and surface area. }
  \item{alpha}{Transparency (0-1). }
}
\details{
Optionaly, uses the 'rgl' package (see \code{\link{plot3d}}), to add a plot of the hull to the current (rgl) device. 
Opens a new device if none is currently open. Uses the non-visible function \code{triangles3d} to plot the hull, 
see details there. 

The convex hull is calculated with the qhull algorithm, see \code{\link{convhulln}} and references therein.
}
\value{
A list with components 'crownvolume' and 'crownsurface', giving the volume and surface 
of the convex hull.
}
\references{ \url{www.qhull.org},

Duursma, R.A., D.S. Falster, F. Valladares, F.J. Sterck, R.W. Pearcy, C.H. Lusk, K.M. Sendall, 
M. Nordenstahl, N.C. Houter, B.J. Atwell, N. Kelly, J.W.G. Kelly, M. Liberloo, 
D.T. Tissue, B.E. Medlyn and D.S. Ellsworth. 2012. Light interception efficiency 
explained by two simple variables: a test using a diversity of small- to medium-sized 
woody plants. New Phytologist. 193:397-408.
}
\author{ Remko Duursma }

\examples{

# Toona example (plant included in package).
crownhull(toona)


# Some xyz data:
coords <- matrix(runif(300,0,1),ncol=3)
library(rgl)
plot3d(coords, col="blue", size=3, axes=FALSE, box=FALSE, xlab="", ylab="", zlab="")
crownhull(coords)

}
\keyword{ misc }
