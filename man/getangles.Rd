\name{getangles}
\alias{getangles}
\title{ Read angles from a .p file.}
\description{
	Reads leaf angles (angle, orientation or azimuth) from a plant file (a Yplant input file with extension \code{.p}, known as a \code{pfile}).
}
\usage{
getangles(plant, whichangle='An.3')
}
\arguments{
  \item{plant}{An object of class \code{plant3d}, or the name of a \code{pfile}.}
  \item{whichangle}{The name of the angle, in quotes (see \code{\link{modifypfile}})}.
}
\details{
If the leaf angle is returned (An.3, the default), all angles are converted so that they are between 0 and 90 degrees.
A warning is printed when any angle > 360, which may indicate problems in the data (this is uncommon).

Other angles may be read, see \code{\link{modifypfile}} for a list of the angles in a pfile.
}
\value{
A vector of angles (in degrees).
}
\author{ Remko Duursma }

\examples{

\dontrun{
# Two options:
# Get leaf angles from a pfile
ang <- getangles("someplant.p")

# Or from a constructed plant:
myplant <- constructplant("someplant.p","someleaf.l")
ang <- getangles(myplant)
}



}
\keyword{ misc }
