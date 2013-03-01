\name{YplantDay}
\alias{YplantDay}
\alias{YplantDay.plant3d}
\alias{YplantDay.plant3dlist}

\title{
Run a simulation over a day with YplantQMC
}
\description{
Interface to daily simulations with YplantQMC. Two objects are required to run the simulation:
a \code{plant3d} object, containing the plant structure information, and a \code{met} object,
containing weather data, solar position, and number of timesteps. 

Optionally, a \code{phy} object is used which contains the leaf gas exchange model for the simulation,
to calculate photosynthesis (and possibly transpiration rate) from light capture and other weather variables.

Also optional is the use of a \code{hemi} object, which specifies shading by a canopy. 

If you don't know where to start, run the example at the bottom of this page.
}
\usage{
\method{YplantDay}{plant3d}(x, met, phy = NULL, hemi = NULL,quiet=FALSE,
writePSR=TRUE, writeOUT=FALSE, ...)
\method{YplantDay}{plant3dlist}(x, met, phy=NULL, hemi=NULL, ...)
}

\arguments{
  \item{x}{An object of class 'plant3d' or 'plant3dlist' (see \code{\link{constructplant}} 
  and \code{\link{readplantlist}}).} 
  \item{met}{An object of class 'ypmet', see \code{\link{setMet}}}
  \item{phy}{An object of class 'ypphy', see \code{\link{setPhy}}}
  \item{hemi}{An object of class 'yphemi', see \code{\link{setHemi}}}
  \item{quiet}{If TRUE, does not write messages to the console.}
  \item{writePSR}{If TRUE, writes a PSR output file.}
  \item{writeOUT}{If TRUE, writes an OUT output file.}
  \item{\dots}{Further arguments passed to \code{\link{runYplant}}}
}
\details{
See the arguments list above for the functions that are used to generate each of the four objects.
Note that the \code{plant} and \code{met} objects are required, and \code{phy} and \code{hemi} are optional.

This function is a user-friendly wrapper for \code{\link{runYplant}}. That function should be used for all
advanced simulations.
}

\value{
The \code{YplantDay} functions returns a list of class \code{yplantsim}, which has \code{print} and
\code{plot} methods (see Examples).

The list has the following components:

\describe{
\item{\code{plant}}{The plant object used in the simulation}
\item{\code{phy}}{If provided, the phy object used in the simulation}
\item{\code{hemi}}{If provided, the hemi object used in the simulation}
\item{\code{outdata}}{A very lengthy dataframe with all results (see below)}
\item{\code{nsteps}}{Number of timesteps}
\item{\code{psrdata}}{Totals and averages by timestep (dataframe), see \code{\link{psrdata}}}
\item{\code{met}}{The met object used in the simulation}
}

The \code{outdata} dataframe in the \code{yplantsim} object lists results for individual leaves,
has the following variables.

\describe{
\item{\code{timeofday}}{Time of day for current timestep (hours)}
\item{\code{leafnr}}{Leaf number}
\item{\code{timestep}}{Length of current timestep (seconds)}
\item{\code{PAR0}}{Above-canopy PAR}
\item{\code{PARleaf}}{Total PAR absorption}
\item{\code{PARdir}}{Direct solar radiation PAR absorption}
\item{\code{PARdiff}}{Diffuse PAR absorption}
\item{\code{reldiff}}{Relative diffuse radiation absorption (0-1).}
\item{\code{reldir}}{Relative direct radiation absorption (0-1).}
\item{\code{LA}}{Individual leaf area (mm2)}
\item{\code{LAproj}}{Projected leaf area (mm2)}
\item{\code{LAsunlit}}{Sunlit, or 'displayed' leaf area (mm2)}
\item{\code{A}}{CO2 assimilation rate (mu mol m-2 s-1)}
\item{\code{E}}{Transpiration rate (mmol m-2 s-1)}
\item{\code{gs}}{Stomatal conductance (mol m-2 s-1)}
\item{\code{A0}}{CO2 assimilation rate for a horizontal unshaded leaf (mu mol m-2 s-1)}
}

Where PAR is photosynthetically active radiation (mu mol m-2 s-1).

The absorptions \code{reldiff} and \code{reldir} are relative to an unshaded horizontal surface.

To extract relative diffuse radiation absorption from an \code{yplantsim} object, for example:
\preformatted{
mysim <- YplantDay(myplant, mymet)
reldif <- mysim$outdata$reldiff
}



}

% \references{
% %% ~put references to the literature/web site here ~
% }
\author{
Remko Duursma
}
% \note{
% }

\seealso{
\code{\link{runYplant}},\code{\link{makereport}}
}
\examples{

\dontrun{
# Set location,
southernfrance <- setLocation(lat=44)

# A daily weather object, use a constant beam fraction of 0.4.
sunnyday <- setMet(southernfrance, month=6, day=21, nsteps=12, Tmin=9, Tmax=29, PARday=22,
	fbeamday=0.4, fbeammethod="constant")

# Light response curve:
toonalrc <- setPhy("lightresponse", 
	leafpars=list(Amax=14.5, Rd=1.4, phi=0.05, theta=0.5, reflec=0.1, transmit=0.05))

# Run YplantQMC for a day. Use the built-in 'largegap' hemiphoto.
toonarun <- YplantDay(toona, sunnyday, toonalrc, largegap)
}

}
\keyword{misc}