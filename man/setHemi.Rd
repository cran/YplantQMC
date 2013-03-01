\name{setHemi}
\alias{setHemi}
\alias{evalHemi}
\alias{plot.yphemi}
\title{
Generate a hemiphoto object
}
\description{
Construct an object that contains information on shading by the canopy, as measured with hemi-spherical photographs ('hemiphotos').
Reads one of two formats. See YplantQMC website for example files, and the instruction manual for more background.

}
\usage{
setHemi(canfile, canopytrans = 0.02)
evalHemi(hemi, altitude = NULL, azimuth = NULL, 
	met = NULL, degrees = TRUE)
\method{plot}{yphemi}(x, met=NULL, sungap=TRUE,
	projection=c("iso","flat"), warn=TRUE, bordercol='black', ...)
}
\arguments{
  \item{canfile}{A canopy file (see Vignette for format example).}
  \item{canopytrans}{Minimum transmission of canopy (when gap fraction = 0).}
  \item{hemi}{An object generated with \code{setHemi}.}
  \item{altitude,azimuth}{Viewing altitude and azimuth.}
  \item{met}{Optionally, a \code{ypmet} object.}
  \item{degrees}{Whether altitude and azimuth were given in degrees (if FALSE, radians).}
  \item{x}{For \code{plot.yphemi}, a \code{yphemi} object}
  \item{sungap}{If TRUE, and a \code{ypmet} object is used, plots the sun along the path, 
with the size relative to the gap fraction}
  \item{projection}{If 'iso', each altitude bin has the same width in the plot (Default). If 'flat', projection is as seen from above. }
  \item{warn}{If TRUE and sungap=TRUE, warns when gap fractio is very low.}
  \item{bordercol}{Color of the grid separating the sky sectors. Use \code{NA} to omit borders.}
  \item{\dots}{Further arguments passed to plot.default}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
% \value{
% %%  ~Describe the value returned
% %%  If it is a LIST, use
% %%  \item{comp1 }{Description of 'comp1'}
% %%  \item{comp2 }{Description of 'comp2'}
% %% ...
% }
\references{
For background on hemiphotos : \url{http://en.wikipedia.org/wiki/Hemispherical_photography}
}
\author{
Remko Duursma
}

\seealso{
\code{\link{setMet}}
}
% \examples{}
\keyword{misc}
