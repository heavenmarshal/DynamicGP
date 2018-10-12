\name{saEI}
\alias{saEI}

\title{Saddlepoint Approximated Expected Improvement Criterion for
  the Squential Design for Inverse Problems}

\description{This function performs the squential design procedure for
  the inverse problem. It starts from an initial design set \code{xi}
  and selects the follow-up design points as per an expected improvement
  (EI) criterion. The values of the EI criterion is numerically
  approximated by the saddlepoint approximation technique. After
  the selection of design points, the solution of the inverse problem is
  estimated either by the \code{ESL2D} approach or \code{SL2D}
  approach. Details are provided in Chapter 4 of Zhang (2018).
}
\usage{
saEI(xi,yi,yobs,nadd,candei,candest,func,...,
     mtype=c("zmean","cmean","lmean"),
     estsol=c("ESL2D","SL2D"),
     frac=.9,gstart=0.0001,
     nthread=4,clutype="PSOCK")
}
\arguments{
  \item{xi}{ An \eqn{N0} by \eqn{d} matrix of \eqn{N0} initial design
    points.
  }

  \item{yi}{ An \eqn{L} by \eqn{N0} response matrix of \code{xi},
    where \eqn{L} is the length of the time series outputs, \eqn{N0} is
    the number of design points.}

  \item{yobs}{ An array of length \eqn{L} of the time-series valued
    field observations or the target response.}

  \item{nadd}{ The number of the follow-up design points selected by
    this function.}

  \item{candei}{An \eqn{M1} by \eqn{d} matrix of \eqn{M1} candidate points
    on which the follow-up design points are selected.}

  \item{candest}{ An \eqn{M2} by \eqn{d} matrix of \eqn{M2} candidate points
    on which the (final) estimated solution to the inverse problem is obtained.
  }

  \item{func}{ An R function of the dynamic computer simulator. The
  first argument of \code{func} should be an array of \eqn{d}-dimensional
  inputs. The simulator \code{func} should returns an array of length
  \eqn{L} as the output.}

\item{...}{ The remaining arguments of the simulator \code{func}}

\item{mtype}{ The type of mean functions for the SVD-based GP models. The choice
    "zmean" denotes zero-mean, "cmean" indicates constant-mean, "lmean" indicates
    linear-mean.}

  \item{estsol}{ The method for estimating the final solution to the inverse
    problem after all follow-up design points are included, "ESL2D"
    denotes the ESL2D approach, "SL2D" denotes the SL2D approach.}

  \item{frac}{ The threshold in the cumulative percentage criterion to select the
    number of SVD bases. The default value is 0.9.
  }

  \item{gstart}{
    The starting number and upper bound of for estimating the nugget
    parameter. If \code{gstart = sqrt(.Machine$double.eps)}, the nugget
    will be fixed at \code{sqrt(.Machine$double.eps)}, since the it is
    the lower bound of the nugget term. The default value is 0.0001.
  }

  \item{nthread}{
    The number of threads (processes) used in parallel execution of this
    function. \code{nthread=1} implies no parallelization. The default
    value is 4.
  }

  \item{clutype}{
    The type of cluster in the R package "parallel" to perform
    parallelization. The default value is "PSOCK". Required only if
    \code{nthread}>1.
  }
}

\value{
  \item{xx}{The design set selected by the sequential design approach,
    which includes both the initial and the follow-up design points}
  \item{yy}{The response matrix collected on the design set \code{xx}}
  \item{xhat}{The estimated solution to the inverse problem obtained on the
    candidate set \code{candest}.}
  \item{maxei}{ An array of length \code{nadd}, it collects the maximum
    value of the EI criterion in each iteration of the sequential design
    approach.}
}

\author{Ru Zhang \email{heavenmarshal@gmail.com},

  C. Devon Lin \email{devon.lin@queensu.ca},

  Pritam Ranjan \email{pritam.ranjan@gmail.com}}

\seealso{\code{\link{ESL2D}}, \code{\link{SL2D}}.}

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
  xi <- lhs::randomLHS(30,3)
  candei <- lhs::randomLHS(1000,3)
  candest <- lhs::randomLHS(1000,3)
  ## evaluate the response matrix on the design matrix
  yi <- apply(xi,1,forretal,timepoints)
  x0 <- runif(3)
  y0 <- forretal(x0,timepoints)
  yobs <- y0+rnorm(200,0,sd(y0)/sqrt(50))
  ret <- saEI(xi,yi,yobs,1,candei,candest,forretal,timepoints,
              nthread=1)
}
\keyword{GP model}
\keyword{Inverse problem}
\keyword{SVD}
