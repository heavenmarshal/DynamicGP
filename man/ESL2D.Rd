\name{ESL2D}
\alias{ESL2D}

\title{
  Expected Square \eqn{L_2} Discrepancy Approach for Estimating the
  Solution to the Inverse Problem
}

\description{This function fits an SVD-based GP model on the
  design set \code{design} and response matrix \code{resp}, and
  minimizes the expected square \eqn{L_{2}} discrepancy on the
  candidate set \code{candidate} to estimate the solution to the inverse
  problem. The detials are provided in Chapter 4 of Zhang (2018).
}
\usage{
ESL2D(design,resp,yobs,candidate,frac=.95,
      mtype=c("zmean","cmean","lmean"),
      gstart=0.0001)
}
\arguments{

  \item{design}{An \eqn{N} by \eqn{d} matrix of \eqn{N}
    training/design inputs.}

  \item{resp}{ An \eqn{L} by \eqn{N} response matrix of \code{design},
    where \eqn{L} is the length of the time series outputs, \eqn{N} is
    the number of design points.}

  \item{yobs}{ A vector of length \eqn{L} of the time-series valued
    field observations or the target response.
  }

  \item{candidate}{ An \eqn{M} by \eqn{d} matrix of \eqn{M} candidate points
    on which the estimated solution to the inverse problem is extracted.
  }

  \item{frac}{
    The threshold in the cumulative percentage criterion to select the
    number of SVD bases. The default value is 0.95.
  }

  \item{mtype}{ The type of mean functions for the GP models. The choice
    "zmean" denotes zero-mean, "cmean" indicates constant-mean, "lmean" indicates
    linear-mean. The default choice is "zmean".
  }
  \item{gstart}{
    The starting number and upper bound of for estimating the nugget
    parameter. If \code{gstart = sqrt(.Machine$double.eps)}, the nugget
    will be fixed at \code{sqrt(.Machine$double.eps)}, since it is
    the lower bound of the nugget term. The default value is 0.0001.
  }
}

\value{
  \item{xhat}{The estimated solution to the inverse problem obtained on the
    candidate set \code{candidate}}.
}

\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritam.ranjan@gmail.com}}

\seealso{\code{\link{SL2D}}, \code{\link{saEI}}.}

\references{ Zhang, R. (2018) \emph{Modeling and Analysis of Dynamic Computer Experiments},
    PhD thesis, Queen's University.}
\examples{
  library("lhs")
  forretal <- function(x,t,shift=1)
  {
    par1 <- x[1]*6+4
    par2 <- x[2]*16+4
    par3 <- x[3]*6+1
    t <- t+shift
    y <- (par1*t-2)^2*sin(par2*t-par3)
  }
  timepoints <- seq(0,1,len=200)
  design <- lhs::randomLHS(30,3)
  candidate <- lhs::randomLHS(1000,3)
  ## evaluate the response matrix on the design matrix
  resp <- apply(design,1,forretal,timepoints)
  x0 <- runif(3)
  y0 <- forretal(x0,timepoints)
  yobs <- y0+rnorm(200,0,sd(y0)/sqrt(50))
  xhat <- ESL2D(design,resp,yobs,candidate)
}
\keyword{GP model}
\keyword{Inverse problem}
\keyword{SVD}
