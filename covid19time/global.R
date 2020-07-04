
### load packages
library(shiny)
library(mgcv)

### load dataset 
brio <- TRUE ### if is to use brazil.io data
if (file.exists('data/wdl.RData')) {
    load('data/wdl.RData')
} else {
    source('rcode/wdata-update.R')
}

### check if the data is updated up to yesterday data
cn <- colnames(wdl[[1]])
Date <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
lastday <- tail(Date, 1)
if (lastday<(Sys.Date()-1)) { ### update data 
    source('rcode/wdata-update.R')
    cn <- colnames(wdl[[1]])
    Date <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
    lastday <- tail(Date, 1)
}

locals <- paste0(ifelse(wdl[[1]]$City=='', '',
                        paste0(wdl[[1]]$City, ', ')),
                 ifelse(wdl[[1]]$Province.State=='', '',
                        paste0(wdl[[1]]$Province.State, ' - ')),
                 wdl[[1]]$Country.Region)
olocals <- sort(locals)

llocals <- as.character(wdl[[1]]$Country.Region)
ib <- which((wdl[[1]]$City=='') & (wdl[[1]]$Province.State!=''))
llocals[ib] <- paste0(wdl[[1]]$Province.State[ib], ' - ',
                      wdl[[1]]$Country.Region[ib])
ibb <- which(nchar(llocals[ib])>25)
llocals[ib[ibb]] <- paste0(wdl[[1]]$Province.State[ib[ibb]], '\n',
                           wdl[[1]]$Country.Region[ib[ibb]])
ib <- which((wdl[[1]]$City!=''))
llocals[ib] <- paste0(wdl[[1]]$City[ib], ' - ',
                      wdl[[1]]$Province.State[ib])
ibb <- which(nchar(llocals[ib])>25)
llocals[ib[ibb]] <- paste0(wdl[[1]]$City[ib[ibb]], '\n',
                           wdl[[1]]$Province.State[ib[ibb]])

sqrtR <- function(x, inverse=FALSE) {
  ineg <- which(x<0)
  ipos <- which(x>0)
  if (inverse) {
      x[ineg] <- (-1*((-1*x[ineg])^2))
      x[ipos] <- x[ipos]^2
  } else {
      x[ineg] <- (-1*sqrt(-1*x[ineg]))
      x[ipos] <- sqrt(x[ipos])
  }
  return(x)
}

logR <- function(x, base=exp(1), a=2, inverse=FALSE) {
    la <- log(a, base)
    lb <- log(a+1, base)
    r <- la/lb
    if (inverse) {
        ff <- findInterval(x, c(-Inf, -la, 0, la, Inf))
        ii <- which(ff==1)
        if (length(ii)>0)
            x[ii] <- (-1)*(base^(-1*x[ii]))
        ii <- which(ff==2)
        if (length(ii)>0)
            x[ii] <- (-1)*(base^(-1*x[ii]/r))+1
        ii <- which(ff==3)
        if (length(ii)>0)
            x[ii] <- (base^(x[ii]/r))-1
        ii <- which(ff==4)
        if (length(ii)>0)
            x[ii] <- base^x[ii]
    } else {
        ff <- findInterval(x, c(-Inf, -a, 0, a, Inf))
        ii <- which(ff==1)
        if (length(ii)>0)
            x[ii] <- -1*log(-1*x[ii], base)
        ii <- which(ff==2)
        if (length(ii)>0)
            x[ii] <- -1*log(1-1*x[ii], base)*r
        ii <- which(ff==3)
        if (length(ii)>0)
            x[ii] <- log(1+x[ii], base)*r
        ii <- which(ff==4)
        if (length(ii)>0)
            x[ii] <- log(x[ii], base)
    }
    return(x)
}

xTransf <- function(x, transf) 
  switch(transf, 
         none=x, 
         sqrt=sqrtR(x, inverse=FALSE), 
         log2=logR(x, 2, 2, inverse=FALSE), 
         log10=logR(x, 10, 2, inverse=FALSE))

formatB <- function(x, scientific = TRUE) {
  if (scientific) {
    x <- format(x, scientific=TRUE)
    x <- gsub('.00e+', 'e+', x, fixed=TRUE)
    x <- gsub('.0e+', 'e+', x, fixed=TRUE)
    x <- gsub('e+09', 'B', x, fixed=TRUE) 
    x <- gsub('e+08', '00M', x, fixed=TRUE) 
    x <- gsub('e+07', '0M', x, fixed=TRUE) 
    x <- gsub('e+06', 'M', x, fixed=TRUE) 
    x <- gsub('e+05', '00K', x, fixed=TRUE) 
    x <- gsub('e+04', '0K', x, fixed=TRUE)
    x <- gsub('e+03', 'K', x, fixed=TRUE)
    x <- gsub('e+02', '00', x, fixed=TRUE)
    x <- gsub('e+01', '0', x, fixed=TRUE)
    x <- gsub('e+00', '', x, fixed=TRUE)
  } else {
    x <- format(x, scientific=FALSE)
    x <- gsub('00000000', 'B', x, fixed=TRUE) 
    x <- gsub('0000000', '0M', x, fixed=TRUE) 
    x <- gsub('0000000', '0M', x, fixed=TRUE) 
    x <- gsub('000000', 'M', x, fixed=TRUE) 
    x <- gsub('00000', '00K', x, fixed=TRUE) 
    x <- gsub('0000', '0K', x, fixed=TRUE)
    x <- gsub('000', 'K', x, fixed=TRUE)
  }
    return(x)
}

prepareData <- function(slocal) {

    ii <- pmatch(slocal, locals)
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
    
    d <- list(x=Date, y=y, o=o)
    
    d$dy <- apply(d$y, 2, function(y) 
        diff(c(0, y)))
    d$do <- apply(d$o, 2, function(y) 
        diff(c(0, y)))
    
    d$sy <- apply(d$dy, 2, sfit)
    d$so <- apply(d$do, 2, sfit)
    
    d <- Rtfit(d)

    attr(d, 'llocals') <- llocals[ii]    
    return(d) 

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
  yy <- array(0L, c(n1, nl, 2))
  yy[,,1] <- d$dy
  yy[,,2] <- d$do

  ee <- array(0, c(n0 + n1, nl, 2))
  ##  ee[1:n0] <- (1-w)*dtmp$y[1:n0] * exp(-(1:n0))/exp(-1)
  for (k in 1:2)
    for (l in 1:nl) {
      y0 <- yy[, l, k]
      y0[y0<0] <- 0
      for (i in 1:n1) 
        ee[i+1:n0, l, k] <- ee[i+1:n0, l, k] + y0[i] * w 
    }
  ee[ee<0.1] <- 0.1
  ##    ee[1:n1] <- ee[1:n1]*(sum(dtmp$y[1:n1])/sum(ee[1:n1]))
  d$Rtupp <- d$Rtlow <- d$Rt <- array(NA, c(n1, nl, 2)) 

  for (k in 1:2) {
    for (l in 1:nl) {
      ii <- which(yy[, l, k]>=0)
      fRt <- gam(y~s(x), poisson(), 
                 data=list(
                   x = ii, y = yy[ii,l,k]), 
                 offset = log(ee[ii,l,k]))
      d$Rt[ii, l, k] <- exp(predict(fRt))
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

data2plot <- function(d,
                      variables,
                      dateRange, 
                      showPoints,
                      transf, 
                      legpos) {

    if (length(variables)<1)
        stop(safeError(
            'Please select at least one variable!'))
    v <- pmatch(
        variables, 
        c('cases', 'deaths'))

    sxlm <- as.Date(dateRange, '%d/%m/%y')
    if (diff(sxlm)<3) 
        stop(safeError('Too narrow time window!'))

    nd0 <- length(d)
    nl <- ncol(d[[2]])
    if (nl>100) 
      stop(safeError('Too many places!'))
    for (j in 1:4) {
        d[[length(d)+1]] <- d[[3+j]]
        for (l in 1:nl) {
            d[[length(d)]][, l] <- xTransf(
                d[[3+j]][, l], transf)
        }
    }
    names(d)[(nd0+1):length(d)] <-
        paste0(names(d)[4:7], '.plot')

    jj <- which((d$x>=sxlm[1]) & 
                (d$x<=sxlm[2]))
    if (length(jj)<1)
        stop(safeError('No data in this time window!'))
    xlm <- xlm0 <- range(d$x[jj])
    if (legpos=='right') {
      xlm[2] <- xlm0[2] + 0.35*diff(xlm0)
      y.ex <- 1.00
      leg.ncols <- 1
    } else {
      legpos <- 'topleft'
      leg.ncols <- 4
      y.ex <- 1 + log(2+(nl%/%leg.ncols), 10)/2
    }
  
  if (length(v)==2) {
      ylm <- range(d$dy.plot[jj,],
                   d$do.plot[jj,])
    plot(d$x, 
         d$dy.plot[,1], 
         axes=FALSE,
         xlim=xlm, 
         ylim=ylm*c(1, y.ex), 
         type = 'n', 
         xlab='', 
         ylab='Daily counts')
  } else {
    if (v==1) {
      ylm <- range(d$dy.plot[jj, ])
      plot(d$x, 
           d$dy.plot[,1], 
           axes=FALSE, 
           xlim=xlm,
           ylim=ylm*c(1, y.ex), 
           type = 'n', 
           xlab='', 
           ylab='Daily number of cases')
    } else {
      ylm <- range(d$do.plot[jj, ])
      plot(d$x, 
           d$do.plot[,1], 
           axes=FALSE, 
           xlim=xlm,
           ylim=ylm*c(1, y.ex), 
           type = 'n', 
           xlab='', 
           ylab='Daily number of deaths')
    }
  }

    xl <- list(x=pretty(d$x))
    xl$l <- format(xl$x, '%b,%d')
    yab <- par()$usr[3:4]
    if (transf=='none') {
        yl <- list(y=pretty(yab))
        yl$l <- yl$y
    }
    if (transf=='sqrt') {
        yl <- list(y=pretty(yab)) 
        yl$l <- sqrtR(yl$y, inverse=TRUE)
    }
    if (transf=='log2') {
        yl <- list(y=pretty(yab, 10))
        yl$l <- logR(yl$y, base=2, inverse=TRUE)
    }
    if (transf=='log10') {
        b <- findInterval(diff(yab), 0:6)
        yl0 <- switch(as.character(b),
                      '1'=c(1,1.3,1.7,2.2,3,4,5.5,7.5),
                      '2'=c(1,1.3,1.8,2.5,3.6,5,7),
                      '3'=c(1,1.4,2,2.8,4,6),
                      '4'=c(1,1.5,2.3,3.5,5.6),
                      '5'=c(1,1.6,3,5.5),
                      '6'=c(1,2,4), 
                      '7'=c(1,3), 
                      '8'=1)
        if (diff(yab)>7) 
            b <- round(diff(yab))
        yadd0 <- rep(log(c(yl0), 10), b+2) + 
            rep(0:(b+1), each=length(yl0))
        f0 <- floor(ylm[1])
        yl <- list(y=sort(unique(f0+yadd0, logR(c(-1,1), 10))))
        yl$l <- round(logR(yl$y, 10, inverse=TRUE))
    } 
    i.yl <- which(findInterval(yl$y, ylm+c(-1,1)*0.03*diff(ylm))==1)
    axis(2, yl$y[i.yl], formatB(yl$l[i.yl], scientific=FALSE), las=1)
    segments(rep(xlm0[1], length(i.yl)), yl$y[i.yl],
             rep(xlm0[2], length(i.yl)), yl$y[i.yl],
             lty=2, col=gray(0.7, 0.5))

    nl <- ncol(d[[2]])
    if (nl==1) {
        scol <- 'black'
    } else {
        scol <- rgb(1:nl/nl, 1-2*abs(1:nl/nl-0.5), nl:1/nl)
    }
    nn <- cbind(colSums(d$dy[jj, , drop=FALSE]),
                colSums(d$do[jj, , drop=FALSE]))
    if (any(v==1)) {
        for (l in 1:nl) {
          lines(d$x, d$sy.plot[,l], col=scol[l], lwd=2)
          if (showPoints)
            points(d$x, d$dy.plot[,l], 
                   cex=1-log(nl,10)/2, pch=19, col=scol[l])
        }        
        oloc <- order(nn[,1], decreasing=TRUE)
    }
    if (any(v==2)) {
        for (l in 1:nl) {
          lines(d$x, d$so.plot[,l], col=scol[l], lty=2, lwd=3)
          if (showPoints)
            points(d$x, d$do.plot[,l], 
                   cex=1-log(nl,10)/2, pch=8, col=scol[l])
        }
        oloc <- order(nn[,2], decreasing=TRUE)
    }

    if (legpos=='right') {
      polygon(c(xlm0[2], xlm[c(2,2)]+5, xlm0[c(2,2)]),
              ylm[c(1,1,2,2,1)]+c(-1,-1,1,1,-1), 
              col=gray(.999), border=gray(0.999))
    }
    
    lll <- attr(d, 'llocals')
    nnll <- sapply(lll, function(x)
        length(grep('\n', x, fixed=TRUE)))
    legend(legpos, lll[oloc], inset = c(0, -0.05),
           col=scol[oloc], lty=1, lwd=5,
           bty='n', xpd=TRUE,
           y.intersp=1+max(nnll)/4,
           ncol=leg.ncols, bg=gray(0.69))
    
    par(mar=c(2, 3.5, 0, 0.5))
    ylm <- range(1, d$Rtlow[jj,,v], 
                 d$Rtupp[jj,,v], na.rm=TRUE)
    plot(d$x,
         d$Rt[,1,1], 
         xlim=xlm,
         ylim=ylm*c(1, y.ex), 
         type='n', xlab='', las=1,
         ylab='Reproduction number',
         axes=FALSE)

    if (nl==1) {
        shad.col <- gray(0.7, 0.5)
    } else {
        shad.col <- rgb(0.3+0.7*(1:nl/nl),
                        0.3+0.7*(1-2*abs(1:nl/nl-0.5)),
                        0.3+0.7*(nl:1/nl), 0.5)
    }

    for (k in v) {
      for (l in 1:nl) {
          ii <- which(!is.na(d$Rt[, l, k]))
          iid <- which(diff(ii)>1)
          s1 <- c(1, iid+1)
          s2 <- c(iid, length(ii))
          for (s in 1:length(s1)) {
              idx <- ii[s1[s]:s2[s]]
              polygon(c(d$x[idx],
                        rev(d$x[idx]),
                        d$x[idx[1]]), 
                      c(d$Rtlow[idx,l,k],
                        rev(d$Rtupp[idx,l,k]),
                        d$Rtlow[idx[1],l,k]), 
                      col=shad.col[l], border=shad.col[l])
          }
          lines(d$x, d$Rt[, l, k], col=scol[l], lwd=2)
      }
    }

    xl <- list(x=pretty(xlm0))
    xl$x <- xl$x[which(xl$x<=xlm0[2])]
    axis(1, xl$x, 
         format(xl$x, '%b,%d'))
    ylr <- pretty(par()$usr[3:4])
    axis(2, ylr, las=1)
    segments(rep(xlm0[1], length(ylr)), ylr,
             rep(xlm0[2], length(ylr)), ylr,
             lty=2, col=gray(0.7, 0.5))
    segments(xlm0[1], 1, xlm0[2], 1)
    abline(h=par()$usr[4])

    if (legpos=='right') {
      polygon(c(xlm0[2], xlm[c(2,2)]+5, xlm0[c(2,2)]),
              ylm[c(1,1,2,2,1)]+c(-1,-1,1,1,-1), 
              col=gray(.999), border=gray(0.999))
    }

    ix <- tail(idx, 1)
    llr <- paste0(sprintf('%1.2f', d$Rt[ix, , v[1]]), '(',
                  sprintf('%1.2f', d$Rtlow[ix, , v[1]]), ', ',
                  sprintf('%1.2f', d$Rtupp[ix, , v[1]]), ')')
    if (length(v)==2)
        llr <- paste('C: ', llr, '\nD:',
                     paste0(sprintf('%1.2f', d$Rt[ix, , v[2]]), '(',
                            sprintf('%1.2f', d$Rtlow[ix, , v[2]]), '-',
                            sprintf('%1.2f', d$Rtupp[ix, , v[2]]), ')'))
    oloc.r <- order(d$Rt[ix, , v[1]], decreasing=TRUE)
    legend(legpos, llr[oloc.r], inset = c(0, -0.05),
           col=scol[oloc.r], lty=1, lwd=5,
           bty='n', xpd=TRUE,
           y.intersp=1+log(max(nnll)+length(v)+1,10), 
           ncol=leg.ncols, bg=gray(0.69))
  
    return(invisible())
}



