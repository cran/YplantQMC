\name{setLocation}
\alias{setLocation}

\title{
Generate a location object
}
\description{
Construct an object that contains a geographical location given by the latitude, longitude, and (optionally),
the longitude of the nearest timezone border. This object is required in \code{\link{setMet}}.
}
\usage{
setLocation(lat = NA, long = 0, tzlong = NA)
}

\arguments{
  \item{lat}{Latitude (degrees, southern hemisphere negative)}
  \item{long}{Longitue (degrees)}
  \item{tzlong}{Optional. Longitude of the nearest timezone border (degrees).}
}
\details{
If the longitude of the nearest time zone border (\code{tzlong}) is not set, it is assumed that 
all times are in 'local apparent time' (LAT), where maximum solar altitude is reached at noon.
If it is set, it is used to calculate the time offset between clock time and solar time.

If \code{lat} and \code{long} are not given, a location can be selected from a simple map of the world.
The \code{maps} package is needed for this utility.

You can also plot the location on a map of the world (see Examples).
}
\value{
An object of class \code{yplocation}.
}
\author{
Remko Duursma
}


\seealso{
\code{\link{setMet}}
}
\examples{

# Set a location:
sydney <- setLocation(lat=-33.5, long=152, tzlong=150)

\dontrun{
# Set a location from a map:
somewhere <- setLocation()

# Plot locations:
plot(sydney)
}

}
\keyword{misc}
