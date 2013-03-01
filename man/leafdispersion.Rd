\name{leafdispersion}
\alias{leafdispersion}
\title{
Leaf dispersion of 3D plants
}
\description{
This function calculates the leaf dispersion for 3D plants, following Duursma et al. (2012). 

The method is based on the mean distance to k nearest neighbors in 3D. 
The function 'leafdispersion' computes this observed mean distance (\code{Ok}) for a plant 
(an object of class \code{plant3d}), as well as for a square box with randomly distributed 
leaves at the same leaf area density.

}
\usage{
leafdispersion(plant, kneighbors = 5, nreplicate = 10, nleaves=NA, crownvol=NA)
}
\arguments{
\item{plant}{An object of class 'plant3d'.}
\item{kneighbors}{Number of neighbors to be used.}
\item{nreplicate}{For the random distribution, the number of replicates to simulate.}
\item{crownvol,nleaves}{Crown volume and number of leaves - optional. If not provided, they
are calculated from the 'plant' object.}
}

\author{
Remko Duursma
}

\value{
A list with the following components:
\describe{
\item{\code{Ok}}{Observed distance to k nearest neighbors}
\item{\code{Ek_noedge}}{Expected distance to k nearest neighbors, for random distribution; no edge correction.}
\item{\code{Ek_edge}}{As above, but with an edge correction}
\item{\code{Ek_edgeSD}}{Standard deviation among replicates of \code{Ek_edge}}
\item{\code{kneighbors}}{Number of neighbors for distance calculation}
\item{\code{disp_edge}}{Edge-corrected leaf dispersion (as in Duursma et al. 2012).}
\item{\code{disp_noedge}}{Non edge-corrected leaf dispersion}
}



}


\references{
Duursma, R.A., D.S. Falster, F. Valladares, F.J. Sterck, R.W. Pearcy, C.H. Lusk, K.M. Sendall, 
M. Nordenstahl, N.C. Houter, B.J. Atwell, N. Kelly, J.W.G. Kelly, M. Liberloo, 
D.T. Tissue, B.E. Medlyn and D.S. Ellsworth. 2012. Light interception efficiency 
explained by two simple variables: a test using a diversity of small- to medium-sized 
woody plants. New Phytologist. 193:397-408.
}

\examples{

# Leafdispersion for the Toona plant
leafdispersion(toona)

}

\keyword{ misc }
