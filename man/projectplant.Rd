\name{projectplant}
\alias{projectplant}
\alias{print.projectedplant3d}
\alias{plot.projectedplant3d}
\title{
Project plant coordinates onto a viewing plane
}
\description{
Transform all leaf edge coordinates onto a viewplane coordinate system, with the 'z' axis pointing toward the viewer.
}
\usage{
projectplant(plant, azimuth, altitude)
\method{plot}{projectedplant3d}(x, silhouette = FALSE, xlim = NULL, ylim = NULL, 
leaffill = TRUE, leafcol = "forestgreen", zerocenter = FALSE, xlab = "X", ylab = "Y", \dots)

}
\arguments{
  \item{plant}{Object of class 'plant3d' (see \code{\link{constructplant}}.}
  \item{azimuth,altitude}{Azimuth and altitude from which plant is viewed.}
  \item{x}{Object of class 'projectedplant3d'.}
  \item{silhouette}{Add a 2D convex hull (see \code{\link{Silhouette}}).}
  \item{xlim,ylim}{Limits for the X and Y axes.}
  \item{leaffill}{If TRUE, fills leaves with green stuff.}
  \item{leafcol}{If leaffill=TRUE, the color of the leaves.}
  \item{zerocenter}{Whether to shift the plant to X=0 and Y=0.}
  \item{xlab,ylab}{Labels for X and Y axes}
  \item{\dots}{Further parameters passed to plot() (but ignored for print).}
}
% \details{
% }
\value{
Returns an object of class 'projectedplant3d', with components:
\item{leaves}{A list of matrices with the coordinates of the leaf edges. Each matrix has columns
VX,VY and VZ, which are the viewplane coordinates (with the Z axis pointing toward the viewer).}
\item{viewbound}{List of min and max values of coordinates (minx, maxx, miny, etc.).}
\item{viewangle}{Azimuth and altitude used for projection.}
}
\author{
Remko Duursma
}

\seealso{
\code{\link{STARbar}}
}
\examples{

# View a plant from above.
# The 2D convex hull is also plotted (the 'silhouette'), 
# and the area of the hull is printed on the graph.
topview <- projectplant(pilularis, altitude=90, azimuth=180)
plot(topview, leaffil=TRUE, silhouette=TRUE)

}
\keyword{misc}
