\name{zenaz}
\alias{zenaz}

\title{
Calculates position of the sun
}
\description{
Calculates the zenith and azimuth angle of the position of the sun, based mostly on routines from Iqbal (1983). 

}
\usage{
zenaz(year = 2012, 
month = 4, 
day = 1, 
lat = -33.6, 
long = 150.7, 
tzlong = long, 
KHRS = 24, 
timeofday = NA, 
LAT = FALSE)
}


\arguments{
  \item{year}{YYYY - to account for eccentricity (small effect).}
  \item{month}{Month number}
  \item{day}{Day of month number}
  \item{lat}{Latitude, degrees.}
  \item{long}{Longitude, degrees. Optional (only used for local apparent time correction.)}
  \item{tzlong}{Longitude of the nearest timezone border}
  \item{KHRS}{Number of timesteps in a day (optional). See Details.}
  \item{timeofday}{Optional, time of day (in hours) (a vector of any length) to calculate the position of the sun}
  \item{LAT}{Logical (default=FALSE). Are the times of day given in 'local apparent time'?}
}
\details{

By default, it is assumed that the time of day is not given in local apparent time (LAT, also known as 'solar time'). To convert the standard time to LAT, the longitude of the location, and the longitude of the nearest time zone border must be given.

Alternatively, use \code{LAT=TRUE} to specify that the time of day is in LAT (that is, solar maximum occurs exactly at noon).

The user can specify a number of timesteps (\code{KHRS}), so that the solar positions are calculated for the midpoint of each timestep (this is used within YplantQMC). Alternatively, specify \code{timeofday} directly.


}
\value{
A list with the following components:
\describe{
\item{\code{hour}}{Time in decimal hours}
\item{\code{altitude}}{Solar altitude (degrees)}
\item{\code{azimuth}}{Solar azimuth (degrees. N=0, E=90)}
\item{\code{daylength}}{Day length in hours}
\item{\code{sunset}}{Time of sunset (hours)}
\item{\code{zenrad}}{Solar zenith position (radians)}
}
      
        
}
\references{
Iqbal, B., 1983. An Introduction to Solar Radiation. Academic
Press, New York, 386 pp
}
\author{
Remko Duursma, based mostly on original FORTRAN code by Belinda Medlyn.
}
\note{
This routine is no doubt less accurate that the NOAA routines provided by the \code{solarpos}
function in the \code{maptools} package. It is easier to use, though. }

\seealso{
\code{\link{setHemi}}
}

\examples{

# Simple use
zenaz(month=8, day=16, timeofday=12, lat=-33)

# Get half-hourly solar positions
hourpos <- zenaz(month=2, day=16, KHRS=48, lat=-33, long=155, tzlong=150)
 with(hourpos, plot(hour, altitude, type='o',ylab=expression(Altitude~(degree))))

}
\keyword{misc}
