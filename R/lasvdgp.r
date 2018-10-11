lasvdgpWorker <- function(X0, design, resp, n0, nn,
                          nfea = min(1000,nrow(design)),
                          nsvd = nn, nadd = 1,
                          frac = .9, gstart = 0.001,
                          resvdThres = min(5, nn-n0),
                          every = min(5,nn-n0),centralize=FALSE,
                          maxit=100, verb=0, errlog="")
{
    if(!is.matrix(design)) stop("design must be a matrix")
    if(!is.matrix(resp)) stop("resp must be a matrix")
    N <- nrow(design)
    m <- ncol(design)
    if(ncol(resp) != N)
        stop("number of design points and responses are not consistent")
    tlen <- nrow(resp)
    if(!is.matrix(X0) && length(X0) != m)
        stop("illegal form of prediction set")
    if(!is.matrix(X0)) X0 <- matrix(X0,ncol=m)
    if(ncol(X0) != m) stop("dimensions of design and prediction set are not consistent")

    M <- nrow(X0)
    if(nfea > N || nfea <0) stop("illegal nfea")
    if(n0>nfea || n0 <0) stop("illegal n0")
    if(nn<n0 || nn>nfea) stop("illegal nn")
    if(nsvd<n0 || nsvd > nfea) stop("illegal nsvd")
    if(nadd < 0) stop("illegal nadd")
    if(resvdThres < 0) stop("illegal resvdThres")
    if(every < 0) stop("illegal every")
    if(centralize)
    {
        rmean <- apply(resp,1,mean)
        resp <- (resp-rmean)
    }
    out <- .C("lasvdGP_R", as.double(t(X0)), as.double(t(design)),
              as.double(resp), as.integer(M), as.integer(N),
              as.integer(m), as.integer(tlen), as.integer(nn),
              as.integer(n0), as.integer(nfea), as.integer(nsvd),
              as.integer(nadd), as.double(frac), as.double(gstart),
              as.integer(resvdThres), as.integer(every),
              as.integer(maxit), as.integer(verb),as.character(errlog),
              pmean=double(M*tlen), ps2=double(M*tlen), flags=integer(M)) #package = lasvdgp
    pmean <- out$pmean
    ps2 <- out$ps2
    if(centralize) pmean <- pmean+rmean
    ret <- list(pmean=matrix(pmean,nrow=tlen),ps2=matrix(ps2,nrow=tlen),flags=out$flags)
    return(ret)
}
