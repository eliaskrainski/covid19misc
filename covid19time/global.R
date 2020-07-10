
pt <- length(grep('MESSAGES=pt', Sys.getlocale())>0)

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

### check if the data is more than 6 hours old
if (difftime(Sys.time(), 
             attr(wdl, 'Sys.time'), 
             units='hours')>6) {
  source('rcode/wdata-update.R')
}
cn <- colnames(wdl[[1]])
Date <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
lastday <- tail(Date, 1)

### locals to select. Composed by City, State and Country
locals <- paste0(ifelse(
    wdl[[1]]$City=='', '',
    paste0(wdl[[1]]$City, ', ')),
    ifelse(
        wdl[[1]]$Province.State=='', '',
        paste0(wdl[[1]]$Province.State, ' - ')),
    wdl[[1]]$Country.Region)
### Put in alphabetical order 
olocals <- sort(locals)

### Too wide state + country names separated by '\n'
llocals <- as.character(
    wdl[[1]]$Country.Region)
llwide <- nchar(llocals) ### nchar (for text size legend control) 
llrow <- rep(1, length(llocals)) ## lines (1+linebreaks), for legend
ib <- which((wdl[[1]]$City=='') &
            (wdl[[1]]$Province.State!=''))
if (length(ib)>0) {
    llocals[ib] <- paste0(
        wdl[[1]]$Province.State[ib], ' - ',
        wdl[[1]]$Country.Region[ib])
    ibb <- which(nchar(llocals[ib])>20)
    if (length(ibb)) {
        llocals[ib[ibb]] <- paste0(
            wdl[[1]]$Province.State[ib[ibb]], '\n',
            wdl[[1]]$Country.Region[ib[ibb]])
        llrow[ib[ibb]] <- 2
        llwide[ib[ibb]] <- pmax(
            nchar(paste(wdl[[1]]$Province.State[ib[ibb]])), 
            nchar(paste(wdl[[1]]$Country.Region[ib[ibb]])))
        llwide <- nchar(llocals)
    }
}

### Too wide city + state names separated by '\n'
ib <- which((wdl[[1]]$City!=''))
if (length(ib)) {
    llocals[ib] <- paste0(
        wdl[[1]]$City[ib], ' - ',
        wdl[[1]]$Province.State[ib])
    ibb <- which(nchar(llocals[ib])>20)
    if (length(ibb)) {
        llocals[ib[ibb]] <- paste0(
            wdl[[1]]$City[ib[ibb]], '\n',
            wdl[[1]]$Province.State[ib[ibb]])
        llrow[ib[ibb]] <- 2
        llwide[ib[ibb]] <- pmax(
            nchar(paste(wdl[[1]]$City[ib[ibb]])),
            nchar(paste(wdl[[1]]$Province.State[ib[ibb]])))
    }
}

### map real to real as 
###   y = sqrt(x), if x>0
###   y = -sqrt(-x), if x<0 
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

### map real to real as 
###   y = log(x, base), if x>=a
###   y = log(1+x, base)/r, if 0<x<a
###   y = -log(-x, base), if x<(-a)
###   y = -log(1-x, base)/r, if -a<x<=(-a)
### r = log(a, base)/log(a+1,base)
logR <- function(x, base=exp(1), a=1.5, inverse=FALSE) {
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

### real to real transformation
xTransf <- function(x, transf) 
  switch(transf, 
         none=x, 
         sqrt=sqrtR(x, inverse=FALSE), 
         log2=logR(x, 2, 2, inverse=FALSE), 
         log10=logR(x, 10, 2, inverse=FALSE))

axTransfTicks <- function(transf, lim) {
  r <- list(x=pretty(lim, 10))
  if (length(r$l)<10)
    r$x <- pretty(lim, 15)
  r$l <- r$x
  if (transf=='sqrt') {
    r$l <- sqrtR(pretty(lim, 10), inverse=TRUE)
    if (length(r$l)<10)
      r$l <- sqrtR(pretty(lim, 15), inverse=TRUE)
    r$x <- sqrtR(r$l)
  }
  if (transf=='log2') {
    if ((lim[1]>=(-6.65)) & (lim[2]<=6.65)) {
      if (diff(lim)>6.65) {
        r$l <- c(-100, -40, -15, -5, 
                 -2:2, 5, 15, 40, 100) 
      } else {
        r$l <- c(-100, -60, -30, -15, -8, -4, 
                 -2:2, 4, 8, 15, 30, 60, 100) 
      }
    } else {
      r$l <- logR(pretty(lim, 10), 2, inverse=TRUE)
      if (length(r$l)<10)
        r$l <- logR(pretty(lim, 15), 2, inverse=TRUE)
      r$x <- logR(r$l, 2)
    }
  }
  if (transf=='log10') {
    if ((lim[1]>=(-3)) & (lim[2]<=3)) {
      b <- findInterval(diff(lim), c(0, 1.1, 2.5, 4))
      if (b==4)
        r$l <- c(-1000, -100, -10, -1, 0, 1, 10, 100, 1000)
      if (b==3)
        r$l <- c(-1000, -300, -100, -30, -10, -3, 
                 0, 3, 10, 30, 100, 300, 1000)
      if (b==2)
        r$l <- c(-1000, -400, -200, -100, -40, -20, -10, -4, 
                 -2:2, 4, 10, 20, 40, 100, 200, 400, 1000) 
      if (b==1)
        r$l <- c(-1000, -700, -500, -300, -200, -100, 
                 -70, -50, -30, -20, -10, -7, -5, 
                 -3:3, 5, 7, 10, 20, 30, 50, 70, 100, 
                 200, 300, 500, 700, 1000)
      r$x <- logR(r$l, 10)
    } else {
      b <- findInterval(
        diff(lim), c(0, 0.5, 1, 2, 3, 5))
      x0 <- log(
          switch(as.character(b),
                 '1'=c(1.1, 1.2, 1.4, 1.6, 1.8, 
                       2, 2.2, 2.5, 2.8, 3.1, 3.5, 
                       4, 4.5, 5, 5.6, 6.3, 7, 8, 9), 
                 '2'=c(1.2, 1.5, 2, 2.5, 3.2, 4, 5, 6.3, 8),
                 '3'=c(1.4,1.9,2.6,3.6,5,7), 
                 '4'=c(2,4), 
                 '5'=c(3), 
                 '6'=3), 10)
      l0 <- floor(lim[1])
      if (l0<0) {
          r <- list(x=l0:(-1) + rep(1-x0, each=length(l0:(-1))))
          b <- b - length(l0:(-1))
          l0 <- 0
          r$x <- c(r$x, l0:b + rep(x0, each=length(l0:b)))
      } else {
          r <- list(x=l0:b + rep(x0, each=length(l0:b)))
      }
      r$x <- unique(sort(c(
        floor(lim[1]):ceiling(lim[2]), r$x)))
      r$l <- logR(r$x, 10, inverse=TRUE)
      if (diff(lim)>6) {
        r$l <- logR(pretty(lim, 10), 10, inverse=TRUE)
        if (length(r$l)<10)
          r$l <- logR(pretty(lim, 15), 10, inverse=TRUE)
        ll <- (10^pmax(0, nchar(r$l)-1)) * 
          round(r$l/(10^pmax(0, nchar(r$l)-1)))
        if (length(unique(sort(ll)))<8) {
          ll <- (10^pmax(0, nchar(r$l)-2)) * 
            round(r$l/(10^pmax(0, nchar(r$l)-2)))
        }
        r$l <- unique(sort(ll))
        r$x <- logR(r$l, 10)
      }
    }
  }
  return(r)
}

### avoid lots of zeros... or scientific 
formatB <- function(x, scientific = TRUE) {
    if (length(grep('.', x))>0)
        scientific <- TRUE
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
    x <- gsub(' ', '', x, fixed=TRUE)
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

### select the data for the selected local(s) 
###   prepare it:
### 1. differenced series
### 2. R_t computations 
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
        if (pt) {
            stop(safeError('Sem dados na seleção feita!'))
        } else {
            stop(safeError('No data in the selection made!'))
        }
    }
    
    d <- list(x=Date, y=y, o=o)
    
    d$dy <- apply(d$y, 2, function(y) 
        diff(c(0, y)))
    d$do <- apply(d$o, 2, function(y) 
        diff(c(0, y)))
    
    d$sy <- apply(d$dy, 2, sfit)
    d$so <- apply(d$do, 2, sfit)
    
    d <- Rtfit(d)

    attr(d, 'ii') <- ii 
    return(d) 

}

### spline smooth series of non-negative data
sfit <- function(y) {
  y[y<0] <- NA
  ii <- which(!is.na(y))
  dtmp <- list(x=ii, r=y[ii]) 
  sfit <- gam(r ~ s(x), poisson(), data=dtmp)
  r <- y 
  r[ii] <- exp(predict(sfit))
  return(r)
}

### do the R_t computations 
Rtfit <- function(d, a=0.5, b=1) {
    
    pw <- pgamma(0:14, shape=(5/3)^2, scale=3^2/5)
    w <- diff(pw)/sum(diff(pw))
    n0 <- length(w)
    n1 <- nrow(d$dy)
    nl <- ncol(d$dy)
    
    yy <- array(0L, c(n1, nl, 2))
    yy[,,1] <- d$dy
    yy[,,2] <- d$do
    yy[yy<0] <- NA
    
    ee <- array(0, c(n0 + n1, nl, 2))
    ##  ee[1:n0] <- (1-w)*dtmp$y[1:n0] * exp(-(1:n0))/exp(-1)
    for (k in 1:2)
        for (l in 1:nl) {
            y0 <- yy[, l, k]
            ##      y0[y0<0] <- 0
            for (i in which(!is.na(y0))) 
                ee[i+1:n0, l, k] <- ee[i+1:n0, l, k] + y0[i] * w 
        }
    ee[ee<0.1] <- 0.1
    ##    ee[1:n1] <- ee[1:n1]*(sum(dtmp$y[1:n1])/sum(ee[1:n1]))
    d$Rtupp <- d$Rtlow <- d$Rt <- array(NA, c(n1, nl, 2)) 
    
    for (k in 1:2) {
        for (l in 1:nl) {
            ii <- which(!is.na(yy[, l, k]))
            fRt <- gam(y~s(x), poisson(), 
                       data=list(
                           x = ii, y = yy[ii,l,k]), 
                       offset = log(ee[ii,l,k]))
            ytmp <- exp(predict(fRt))*ee[ii,l,k]
### consider gamma(a+y, b+E) with a=2, b=1
            d$Rt[ii, l, k] <- (ytmp + a)/(ee[ii,l,k] + b)
            d$Rt[1:n0, l, k] <- NA
        }
    }
    d$Rt[c(d$dy, d$do)<0] <- NA
    
### IC consider gamma(a+y, b+E) with a=2, b=1
    for (k in 1:2) {
        for (l in 1:nl) {
            ytmp <- d$Rt[,l,k]*ee[1:n1,l,k]
            d$Rtlow[,l,k] <- qgamma(0.025, ytmp+a, ee[1:n1,l,k]+b)
            d$Rtupp[,l,k] <- qgamma(0.975, ytmp+a, ee[1:n1,l,k]+b)
        }
    }
    
    return(d) 
    
}

## display the data and R_t
data2plot <- function(d,
                      variables,
                      dateRange, 
                      showPoints,
                      transf, 
                      legpos) {

    if (length(variables)<1)
        if (pt) {
            stop(safeError(
                'Favor selecionar pelo menos uma variável!'))
        } else {
            stop(safeError(
                'Please select at least one variable!'))
        }
    v <- pmatch(
        variables, 
        c('cases', 'deaths'))

    sxlm <- as.Date(dateRange, '%d/%m/%y')
    if (diff(sxlm)<3) {
        if (pt) {
            stop(safeError(
                'A janela temporal selecionada é muito pequena!'))
        } else {
            stop(safeError('Too narrow time window selected!'))
        }
    }

    nd0 <- length(d)
    nl <- ncol(d[[2]])
    if (nl>100) {
        if (pt) {
            stop(safeError('Muitos lugares selecionados!'))
        } else {
            stop(safeError('Too many places!'))
        }
    }
    
    for (j in 1:4) {
        d[[length(d)+1]] <- d[[3+j]]
        for (l in 1:nl) {
            d[[length(d)]][, l] <- xTransf(
                d[[3+j]][, l], transf)
        }
    }
    names(d)[(nd0+1):length(d)] <-
        paste0(names(d)[4:7], '.plot')

    jj0 <- which((d$x>=sxlm[1]) & 
                 (d$x<=sxlm[2]))
    rjj <- apply(d$dy, 2, function(x) {
      range(jj0[which(!is.na(x[jj0]))])
    })
    jj <- min(rjj):max(rjj)
    
    if (length(jj)<1) {
        if (pt) {
            stop(safeError('Sem dados nessa janela temporal!'))
        } else {
            stop(safeError('No data in this time window!'))
        }
    }
    
    lll <- llocals[attr(d, 'ii')]
    nnll <- llrow[attr(d, 'ii')]
    print(list(ii=attr(d, 'ii'),
               lll=lll, nnll=nnll))
    
    xlm <- xlm0 <- range(d$x[jj])
    if (legpos=='right') {
      xlm[2] <- xlm0[2] + 0.4*diff(xlm0)
      y.ex2 <- y.ex1 <- 0.00
      leg.ncols <- 2
    } else {
      legpos <- 'topleft'
      leg.ncols <- 5
      y.ex1 <- (max(nnll) + length(v)) * 
        ((nl+leg.ncols-1)%/%leg.ncols)*0.15
      y.ex2 <- length(v) * 
        ((nl+leg.ncols+1)%/%(leg.ncols+2))*0.15
    }

    if (pt) {
        ylabs <- list(c(
            'Contagem diária',
            'Número diário de casos',
            'Número diário de óbitos'),
            'Número de reprodução')
    } else {
        ylabs <- list(
            c('Daily counts',
              'Daily number of cases',
              'Daily number of deaths'),
            'Reproduction number')
    }
  
  if (length(v)==2) {
      ylm <- range(d$dy.plot[jj,],
                   d$do.plot[jj,], na.rm=TRUE)
    plot(d$x, 
         d$dy.plot[,1], 
         axes=FALSE,
         xlim=xlm, 
         ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
         type = 'n',
         xlab='',
         ylab=ylabs[[1]][1]) 
  } else {
    if (v==1) {
      ylm <- range(d$dy.plot[jj, ], na.rm=TRUE)
      plot(d$x, 
           d$dy.plot[,1], 
           axes=FALSE, 
           xlim=xlm,
           ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
           type = 'n', 
           xlab='', 
           ylab=ylabs[[1]][2])
    } else {
      ylm <- range(d$do.plot[jj, ], na.rm=TRUE)
      plot(d$x, 
           d$do.plot[,1], 
           axes=FALSE, 
           xlim=xlm,
           ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
           type = 'n', 
           xlab='', 
           ylab=ylabs[[1]][3])
    }
  }
    
    xl <- list(x=pretty(xlm0, 10))
    if (length(xl$x)<10)
      xl <- list(x=pretty(xlm0, 20))
    xl$l <- format(xl$x, '%b,%d')
    xl$x <- xl$x[which(xl$x<=(xlm0[2] + 1))]

    yl <- axTransfTicks(transf, ylm)
    yab <- par()$usr[3:4]
    i.yl <- which(findInterval(
        yl$x, ylm+c(-1,1)*0.05*diff(ylm))==1)
    
    axis(2, yl$x[i.yl], round(yl$l[i.yl]), las=1)
    segments(rep(xlm0[1], length(i.yl)), yl$x[i.yl],
             rep(xlm0[2], length(i.yl)), yl$x[i.yl],
             lty=2, col=gray(0.7, 0.5))

    nl <- ncol(d[[2]])
    if (nl==1) {
        scol <- 'black'
    } else {
        scol <- rgb(1:nl/nl, 1-2*abs(1:nl/nl-0.5), nl:1/nl)
    }
    nn <- cbind(colSums(d$dy[jj, , drop=FALSE], na.rm=TRUE),
                colSums(d$do[jj, , drop=FALSE], na.rm=TRUE))
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

    if (length(v)==2) {
      nlab <- paste0(nn[1:nl], ' C, ', 
                     nn[nl + 1:nl], ' D')
    } else {
      nlab <- nn[nl*(v-1) + 1:nl]
    }
    lll <- paste0(lll, '\n', nlab)
    legend(legpos, lll[oloc], inset = c(0, -0.05),
           col=scol[oloc], lty=1, lwd=5,
           bty='n', xpd=TRUE,
           y.intersp=log(length(v)+max(nnll), 2),
           cex=1-log(length(v)+max(nnll), 10)/2,
           ncol=leg.ncols)
    
    par(mar=c(2, 4.5, 0, 0.5))
    ylm <- range(1, d$Rtlow[jj,,v], 
                 d$Rtupp[jj,,v], na.rm=TRUE)
    plot(d$x,
         d$Rt[,1,1], 
         xlim=xlm,
         ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex2), 
         type='n', xlab='', las=1,
         ylab=ylabs[[2]], 
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
          lines(d$x, d$Rt[, l, k], col=scol[l], lwd=2, lty=k)
      }
    }

    axis(1, xl$x, 
         format(xl$x, '%b,%d'))
    ylr <- pretty(ylm)
    axis(2, ylr, las=1)
    segments(rep(xlm0[1], length(ylr)), ylr,
             rep(xlm0[2], length(ylr)), ylr,
             lty=2, col=gray(0.7, 0.5))
    segments(xlm0[1], 1, xlm0[2], 1)
    abline(h=par()$usr[4])

    getL <- function(x)  x[tail(which(!is.na(x)))]
    rt.last <- apply(d$Rt[jj, , , drop=FALSE], 3, apply, 2, getL)
    rtl.last <- apply(d$Rtlow[jj, , , drop=FALSE], 3, apply, 2, getL)
    rtu.last <- apply(d$Rtupp[jj, , , drop=FALSE], 3, apply, 2, getL)

    llr <- paste0(
      sprintf('%1.2f', rt.last[, v[1]]), '(',
      sprintf('%1.2f', rtl.last[, v[1]]), ', ',
      sprintf('%1.2f', rtu.last[, v[1]]), ')')[oloc]
    if (length(v)==2) {
      llr <- c(llr, 
               paste0(sprintf('%1.2f', rt.last[, v[2]]), '(',
                      sprintf('%1.2f', rtl.last[, v[2]]), ', ',
                      sprintf('%1.2f', rtu.last[, v[2]]), ')'
                      )[oloc])
      iill <- rep(1:nl, each=length(v)) + 
        rep(c(0, nl), nl)
      llr <- llr[iill]
    }
    legend(legpos, llr, 
           col=rep(scol[oloc], each=length(v)), 
           lty=rep(1:length(v), nl), lwd=2,
           bty='n', xpd=TRUE, cex=0.85, 
           ncol=leg.ncols+2*(legpos!='right'))
  
    return(invisible())
}
