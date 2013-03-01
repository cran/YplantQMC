\name{makereport}
\alias{makereport}

\title{
Make a PDF report of several YplantQMC objects
}
\description{
Produce a report containing standard graphs and summaries of YplantQMC objects.
See Details for usage.
}
\usage{
makereport(plant = NULL, 
	phy = NULL, 
	met = NULL, 
	hemi = NULL, 
	ypsim = NULL, 
	filename = NA)
}
\arguments{
  \item{plant}{An object of class 'plant3d', see \code{\link{constructplant}}}
  \item{phy}{An object of class 'ypphy', see \code{\link{setPhy}}}
  \item{hemi}{An object of class 'yphemi', see \code{\link{setHemi}}}
  \item{met}{An object of class 'ypmet', see \code{\link{setMet}}}
  \item{ypsim}{An object of class 'yplantsim', see \code{\link{YplantDay}}}
  \item{filename}{Optional, the name of the output file}
}
\details{
This function produces a number of standard plots and prints of five different YplantQMC objects.
The plots cannot at the moment not be customized; please see below for the component functions 
that generate the plots (these usually have more options for customization).
Or, modify the code of \code{makereport} as you see fit.

To create a report, simply use this command:
\preformatted{
	makereport(plant=myplant, met=asunnyday, 
		phy=eucleaf, hemi=mycanopy, ypsim=eucsim1)
}
Where myplant, asunnyday, eucleaf, mycanopy and eucsim1 are objects that you have already generated.
The function is flexible : you can generate a report on a subset of the objects, for example
only on the hemiphoto and the plant:
\preformatted{
	makereport(plant=myplant, hemi=spruceforest)
}

You may want to check out the following functions, which are used in \code{makereport}:
\describe{
\item{\code{\link{viewplot}}}{Produces a three-panel plot with side and top views of the plant.}
\item{\code{\link{summary.plant3d}}}{Summarizes a plant}
\item{\code{\link{setHemi}}}{Reads a hemiphoto - also describes the plot function.}
\item{\code{\link{fitdistribution}}}{Fits (and plots) a leaf angle distribution.}
\item{\code{\link{setMet}}}{Constructs (and plots) a weather object.}
\item{\code{\link{plot.leaffile}}}{Plots a leaf.}
\item{\code{\link{YplantDay}}}{A daily Yplant simulation (and a standard plot).}
}
}

\note{
Note that \code{makereport} might fail if you have a PDF open with the same name (as you may have 
generated this report once before on the same day). Make sure to close the PDF before
running \code{makereport}. If a PDF is generated that can't be opened, use this command:
\preformatted{
dev.off()
}
And try again. 

}

\value{
A PDF is generated in the current working directory. 
}
\author{
Remko Duursma
}
\seealso{
\code{\link{plot.plant3d}},\code{\link{viewplot}},\code{\link{YplantDay}}
}
\keyword{misc}
