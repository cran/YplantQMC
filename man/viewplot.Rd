\name{viewplot}
\alias{viewplot}

\title{
Make a three panel plot of a 3D plant
}
\description{
Three plots of a 3D plant: views from the east, south and from above.
This is a lame way to plot the plant, as the stems are always plotted on top
(whether or not they are visible). It is available for quick plotting, 
and for \code{\link{makereport}}, as it does not require the \code{rgl} package.

See \code{\link{plot.plant3d}} for more advanced, high quality, plotting of plants.


}
\usage{
viewplot(plant, side=c("east","south","above"), stems=TRUE, autopar=TRUE)
}
\arguments{
  \item{plant}{An object of class 'plant3d' (see \code{\link{constructplant}}).}
  \item{side}{Which side to plot (can specify more than 1).}
  \item{stems}{If TRUE, plots the stem sections (always on top, lame).}
  \item{autopar}{If TRUE, tries to guess how to split up the plotting device.}
}
\details{This function plots the plant from above, east and west views. Stems are also plotted,
as opposed to the standard plot of a projected plant (see \code{\link{projectplant}}).
}
\author{
Remko Duursma
}
\note{
This function is called by \code{\link{makereport}}.
}

\seealso{
\code{\link{plot.plant3d}},\code{\link{projectplant}}
}

\examples{

# Toona australis from above
viewplot(toona, "above")



}

\keyword{misc}
