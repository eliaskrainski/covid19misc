library(shiny)
library(mgcv)

l0 <- log(c(1, 3), 10)
ll <- vector('list', 5)
ll[[5]]$y <- c(
  rep(-8:-1, each=2)-rev(l0), -l0[2],
  -0.15, 0, 0.15, l0[2],
  rep(1:8, each=2)+l0)
ll[[5]]$l <- paste0(
  c(rep(c(-300, -100, -30, -10, -3, -1), 3), 0,
    rep(c(1, 3, 10,30, 100,300), 3)),
  rep(c('M', 'K', '', 'K', 'M'), c(6, 6, 13, 6, 6)))
ll[[4]] <- lapply(ll[[5]], function(x) 
  x[seq(2, length(x), 2)])
ll[[3]] <- lapply(ll[[5]], function(x) 
  x[seq(1, length(x), 3)])
l0 <- log(c(1, 2, 5), 10)
ll[[2]] <- list(
  y=c(rep(-8:-1, each=3)-rev(l0),
      -l0[3:2], -0.15, 0, 0.15, l0[2:3],
      rep(1:8, each=3)+l0))
ll0 <- c(1, 2, 5, 10, 20, 50, 100, 200, 500)
ll[[2]]$l <- paste0(
  c(rep(-rev(ll0), 3), 0, rep(ll0, 3)),
  rep(c('M', 'K', '', 'K', 'M'),
      c(9, 9, 19, 9, 9)))

if (file.exists('../data/wdl.RData')) {
  load('../data/wdl.RData')
} else {
   setwd("..")
   source('rcode/wdata-update.R')
   setwd("covid19time")
}
cn <- colnames(wdl[[1]])
Date <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
lastday <- tail(Date, 1)
if (lastday<(Sys.Date()-2)) {
   setwd("..")
   source('rcode/wdata-update.R')
   setwd("covid19time")
   cn <- colnames(wdl[[1]])
   Date <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
   lastday <- tail(Date, 1)
}

#wc.names <- levels(wdl[[1]]$Country.Region)
#st.names <- setdiff(levels(wdl[[1]]$Province.State), 'total')
#mu.names <- setdiff(levels(wdl[[1]]$City), 'total')
wc.names <- wdl[[1]]$Country.Region
st.names <- wdl[[1]]$Province.State
st.names <- ifelse(st.names=='', '', paste0(st.names, ' - '))
mu.names <- wdl[[1]]$City
mu.names <- ifelse(mu.names=='', '', paste0(mu.names, ', '))

locals <- sort(paste0(mu.names, st.names, wc.names))

sqrtR <- function(x) {
  ineg <- which(x<0)
  ipos <- which(x>0)
  x[ineg] <- (-1*sqrt(-1*x[ineg]))
  x[ipos] <- sqrt(x[ipos])
  return(x)
}

logR <- function(x, base=exp(1), a=2) {
  r <- log(a, base)/log(a+1, base)
  kk <- findInterval(x, c(-Inf, -a, 0, a, Inf))
  ii <- which(kk==1)
  x[ii] <- -1*log(-1*x[ii], base)
  ii <- which(kk==2)
  x[ii] <- -1*log(1-1*x[ii], base)*r
  ii <- which(kk==3)
  x[ii] <- log(1+x[ii], base)*r
  ii <- which(kk==4)
  x[ii] <- log(x[ii], base)
  return(x)
}

xTransf <- function(x, transf) 
  switch(transf, 
         none=x, 
         sqrt=sqrtR(x), 
         log2=logR(x, 2), 
         log10=logR(x, 10))

selectData <- function(input) {
   ii <- which(locals %in% input$local)
   jj <- 7:ncol(wdl[[1]])
   y <- as.matrix(wdl[[1]][ii, jj, drop=FALSE])
   ii0 <- which(colSums(y>0)>0)
   if (length(ii0)>0) {
      o <- t(wdl[[2]][ii, jj[ii0[1]:ncol(y)]])
      Date <- Date[ii0[1]:ncol(y)]
      y <- t(y[, ii0[1]:ncol(y), drop=FALSE])
   } else {
      stop(safeError('Sem dados na seleção feita!'))
   }
   return(list(x=Date, y=y, o=o))
}

sfit <- function(y) {
  y[y<0] <- NA
  ii <- which(!is.na(y))
  dtmp <- list(x=ii, r=y[ii]) 
  sfit <- gam(r ~ s(x), poisson(), data=dtmp)
  r <- y 
  r[ii] <- exp(predict(sfit))
  return(r)
}

Rtfit <- function(d) {
  pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
  w <- diff(pw)/sum(diff(pw))
  n0 <- length(w)
  n1 <- nrow(d$dy)
  nl <- ncol(d$dy)
  rr <- array(0L, c(n1, nl, 2))
  rr[,,1] <- d$dy
  rr[,,2] <- d$do
  rr[rr<0] <- 0
  ee <- array(0, c(n0 + n1, nl, 2))
  ##  ee[1:n0] <- (1-w)*dtmp$y[1:n0] * exp(-(1:n0))/exp(-1)
  for (k in 1:2)
    for (l in 1:nl)
      for (i in 1:n1) 
        ee[i+1:n0-1, l, k] <- ee[i+1:n0-1, l, k] + rr[i, l, k] * w 
  ee[ee<0.1] <- 0.1
  ##    ee[1:n1] <- ee[1:n1]*(sum(dtmp$y[1:n1])/sum(ee[1:n1]))
  d$Rtupp <- d$Rtlow <- d$Rt <- array(0, c(n1, nl, 2)) 
  x0 <- as.numeric(d$x-min(d$x))
  for (k in 1:2) {
    for (l in 1:nl) {
      fRt <- gam(y~s(x), poisson(), 
                 data=list(
                   x=x0, y=rr[,l,k], 
                   lE=log(ee[1:n1,l,k])), 
                 offset=lE)
      d$Rt[, l, k] <- exp(predict(fRt))
      d$Rt[1:n0, l, k] <- NA
    }
  }
  d$Rt[c(d$dy, d$do)<0] <- NA
  for (k in 1:2) {
    for (l in 1:nl) {
      d$Rtlow[,l,k] <- qgamma(
        0.025, d$Rt[, l, k]*ee[1:n1, l, k], 1)/ee[1:n1, l, k]
      d$Rtupp[,l,k] <- qgamma(
        0.975, d$Rt[, l, k]*ee[1:n1, l, k], 1)/ee[1:n1, l, k]
    }
  }
  return(d) 
}

prepareData <- function(d, transf) {
  d$dy <- apply(d$y, 2, function(y) 
    diff(c(0, y)))
  d$do <- apply(d$o, 2, function(y) 
    diff(c(0, y)))
  d$sy <- apply(d$dy, 2, sfit)
  d$so <- apply(d$do, 2, sfit)
  print(str(d))
  if (FALSE) {
  d <- Rtfit(d)
  print("OK RFIT")
  print(str(d))
  }
  for (j in 2:5)
    for (l in 1:ncol(d[[j]]))
      d[[j]][, l] <- xTransf(d[[j]][,l], transf)
  print(str(d))
  return(d) 
}

data2plot <- function(
  d, v, 
  xlim=NULL, 
  transf, 
  scol) {
  if (!is.null(xlim)) {
    jj <- which((d$x>=xlim[1]) & 
                  (d$x<=xlim[2]))
    if (length(jj)<1) 
      stop(safeError('No data in this time window!'))
    d$x <- d$x[jj]
    for (l in 2:5)
      d[[l]] <- d[[l]][jj, , drop=FALSE]
  }
  if (length(v)==2) {
    ylm <- range(d$dy, d$do)
    plot(d$x, 
         d$dy[,1], 
         axes=FALSE, 
         ylim=ylm*c(1, 1.1), 
         type = 'n', 
         xlab='', 
         ylab='Daily counts.')
  } else {
    if (v==1) {
      ylm <- range(d$dy)
      plot(d$x, 
           d$dy[,1], 
           axes=FALSE, 
           ylim=ylm*c(1, 1.1), 
           type = 'n', 
           xlab='', 
           ylab='Daily number of cases.')
    } else {
      ylm <- range(d$do)
      plot(d$x, 
           d$do[,1], 
           axes=FALSE, 
           ylim=ylm*c(1, 1.1), 
           type = 'n', 
           xlab='', 
           ylab='Daily number of deaths.')
    }
  }
  axis(1, pretty(d$x), 
       format(pretty(d$x), '%b,%d'))
  y0 <- pretty(par()$usr[3:4])
  if (transf=='none')
    axis(2, y0)
  if (transf=='sqrt')
    axis(2, y0, format(y0^2, digits=1)) 
  if (transf=='log2') {
    axis(2, y0, format(2^y0, digits=1)) 
  }
  if (transf=='log10') {
    axis(2, y0, format(10^y0, digits=1))
  }
  nl <- ncol(d$dy)
  for (l in 1:nl) {
    if (any(v==1)) {
      points(d$x, d$dy[,l], pch=19, col=scol[l])
      lines(d$x, d$sy[,l], col=scol[l])
    }
    if (any(v==2)) {
      points(d$x, d$do[,l], pch=8, col=scol[nl+l])
      lines(d$x, d$so[,l], col=scol[nl+l])
    }
  }
  if (FALSE) {
  plot(d$x, d$Rt[,1,1], 
       ylim=range(1, 
                  d$Rtlow[,,v], 
                  d$Rtupp[,,v], na.rm=TRUE), 
       type='n', xlab='', 
       ylab='Reproduction number')
  if (nl==1) {
    shad.col <- gray(0.7, 0.5)
  } else {
    shad.col <- rgb(1:nl/nl, 1-2*abs(1:nl/nl-0.5), nl:1/nl, 0.5)
  }
  for (k in v)
    for (l in 1:nl) {
      polygon(c(d$x, rev(d$x), d$x[1]), 
              c(d$Rtlow[,l,k], rev(d$Rtupp[,l,k]), d$Rtlow[1,l,k]), 
              col=shad.col[l], border=shad.col[l])
      lines(d$x, d$Rt[, l, k], col=scol[l])
    }
  }
  return(invisible())
}


Rtplot <- function(d, xlim=NULL) {
  if (!is.null(xlim)) {
    jj <- which((d$x>=xlim[1]) & 
                  (d$x<=xlim[2]))
    if (length(jj)<1) 
      stop(safeError('No data in this time window!'))
    d$x <- d$x[jj]
    for (k in 3:length(d))
      d[[k]] <- d[[k]][jj, , drop=FALSE]
    
  }
  
}