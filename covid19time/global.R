
pt <- length(grep('MESSAGES=pt', Sys.getlocale())>0)
npretty <- 7

### load packages
library(shiny)
library(splines)
###library(mgcv)

mgcv.ok <- FALSE
if (FALSE)
    mgcv.ok <- require(mgcv)

### load dataset 
wupdate <- FALSE 
if (file.exists('data/wdl.RData')) {
  load('data/wdl.RData')
  if (difftime(Sys.time(),
               attr(wdl, 'Sys.time'),
               units='hours')>71) {
    wupdate <- TRUE
  }
}

if(wupdate) {
  system('R CMD BATCH --vanilla rcode/wdl-update.R')
  load('data/wdl.RData')
  system('R CMD BATCH --vanilla rcode/wgmbl-update.R')
  system('R CMD BATCH --vanilla rcode/wambl-update.R')
  system('R CMD BATCH --vanilla rcode/wvac-update.R')
}

load('data/wgmbl.RData')
load('data/wambl.RData')
load('data/wvac.RData')

names(wvac) <- paste('Dose', names(wvac))
att2 <- attributes(wvac)[-1]
wvac <- wvac[1:2]
attributes(wvac) <- c(attributes(wvac)[1], att2)

cn <- colnames(wdl[[1]])
vecDate <- as.Date(cn[7:length(cn)], 'X%Y%m%d')
lastday <- tail(vecDate, 1)

### total in the world
iisel <- which(wdl[[1]]$Province.State=='')
if (any(wdl[[1]]$Country.Region[iisel]=='Brasil') &
    any(wdl[[1]]$Country.Region[iisel]=='Brazil')) {
    iisel <- setdiff(iisel, 
                     which(wdl[[1]]$Country.Region=='Brazil'))
}

nn.show <- format(sapply(wdl, function(m) {
    mm <- m[iisel, -5:0+ncol(m)]
    mm[is.na(mm)] <- 0
    for (j in 2:ncol(mm)) {
        ii <- which(mm[,j]<mm[,j-1])
        if (length(ii)>0)
            mm[ii, j] <- mm[ii, j-1]
    }
    sum(mm[, j], na.rm=TRUE)
}), big.mark = ',')

lb.n <- list('cases', 'deaths')
if (pt) {
    nn.show <- gsub(',', '.', nn.show, fixed=TRUE)
    names(lb.n) <- 
        c(paste0('Casos (', nn.show[1], ')'), 
          paste0('Óbitos (', nn.show[2], ')'))
    ylmob <- 'Mobilidade'
} else {
    names(lb.n) <- 
        c(paste0('Cases (', nn.show[1], ')'), 
          paste0('Deaths (', nn.show[2], ')'))
    ylmob <- 'Mobility'
}

allpls <-
    enpls <- c(
        'Daily counts',
        'Accumulated count',
        'Reproduction number', 
        'Fatality rate (%)',
        names(wvac), names(wgmbl), names(wambl))
if (pt) {
    allpls <- c(
        'Contagem diária',
        'Contagem acumulada',
        'Número de reprodução', 
        'Taxa de letalidade (%)',
        names(wvac), 
        'varejo e recreação',
        'supermercados e farmácias',
        'parques', 'estações de transporte',
        'locais de trabalho', 'residências',
        'dirigindo', 'trânsito', 'andando')
} 

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

glocals0 <- gsub('___', '', rownames(wgmbl[[1]]), fixed=TRUE)
glocals0 <- gsub('__', '', glocals0, fixed=TRUE)
igl_ <- which(substr(glocals0, 1, 1)=='_') 
gl_ <- substring(glocals0[igl_], 2)
glocals0[igl_] <- gl_
g_ <- gregexpr('_', glocals0)

glocals <- sapply(1:length(g_), function(i) {
    if (any(g_[[i]]<0)) return(glocals0[i])
    nj <- length(g_[[i]])
    r <- glocals0[i]
    if (nj==1)
        r <- paste0(substr(r, 1, g_[[i]]-1), ' - ',
                    substring(r, g_[[i]]+1))
    if (nj==2)
        r <- paste0(substr(r, 1, g_[[i]][1]-1), ', ',
                    substr(r, g_[[i]][1]+1, g_[[i]][2]-1), ' - ',
                    substring(r, g_[[i]][2]+1))
    return(r)
})

ulocals <- sort(unique(c(olocals, glocals)))

alocals0 <- lapply(wambl, function(m) attr(m, 'local'))
alocals <- lapply(alocals0, function(x) {
    xx <- Reduce('rbind', strsplit(x, '_'))
    x1 <- ifelse(xx[,1]=='', '', paste0(xx[,1], ', '))
    x2 <- ifelse(xx[,2]=='', '', paste0(xx[,2], ' - '))
    paste0(x1, x2, xx[,3])
})

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

y7fun <- function(y) {
    r <- y
    aux <- y[1:7]
    r[1:3] <- (y[1:3] + sum(aux[aux>0])/7)/2
    for (j in 4:(length(y)-3)) {
        aux <- y[-3:3+j]
        r[j] <- sum(aux[aux>0])/7
    }
    for (j in (length(y)-2):length(y)) {
        aux <- y[(j-6):j]
        r[j] <- (y[j]*(y[j]>0)+sum(aux[aux>0])/7)/2
    }
    return(r)
}

y2positive <- function(y) {
    y7 <- y7fun(y)
    ii <- which(y<0)
    for(i in ii) {
        j0 <- -21:21 + i
        j0 <- j0[j0>0]
        for(r in seq(3, 1, -0.1)) {
            mm <- r*mean(y7[j0])
            jj <- which(y[j0]>mm)
            if(length(jj)>0) {
                d <- floor(y[j0[jj]] -mm)
            } else d <- 0
            if((sum(d)+y[i])>0) 
                break
        }
        o <- order(d, decreasing = TRUE)
        for (j in o) {
            dj <- d[j]
            if ((dj+y[i])>0) {
                dj <- -y[i]
            }
            y[i] <- y[i] + dj
            jo <- j0[jj[j]]
            y[jo] <- y[jo]-dj
            if(y[i]>=0) break
        }
    }
    return(y)
}

tSmoothPoisson <- function(y, X, B) {
    n <- length(y)
    if (is.null(colnames(X)))
        colnames(X) <- paste0('x', 1:ncol(X))
    if (is.null(colnames(B)))
        colnames(B) <- paste0('b', 1:ncol(B))
    ix <- which(diag(crossprod(X))>0)
    nx <- length(ix) 

    if ((length(ix)>0) & (n>(3*nx))) {
        X <- X[, ix, drop = FALSE]
    } else {
        X <- matrix(1, n, 1)
        colnames(X) <- 'x0'
    }

    ib <- which(diag(crossprod(B))>0)
    nb <- length(ib) 
    if (nb>0)
        B <- B[, ib, drop = FALSE]

    if (n>(3*(nx+nb))) {
        res <- glm.fit(
            cbind(X, B), y, 
            family=poisson())
    } else {
        res <- glm.fit(
            X, y, 
            family=poisson())
    }
    bg <- res$coef
    
    bg <- bg[!is.na(bg)]
    icx <- pmatch(colnames(X), names(bg))
    iicx <- which(!is.na(icx))
    
    icb <- pmatch(colnames(B), names(bg))
    iicb <- which(!is.na(icb))

    if(length(iicx)>0) {
        res$x.m <- drop(
            X[, iicx, drop=FALSE] %*% bg[icx[iicx]])
    } else {
        res$x.m <- rep(log(mean(y)), n)
    }

    if (length(iicb)>0) {
        res$b.m <- drop(
            B[, iicb, drop=FALSE] %*% bg[icb[iicb]])
    } else {
        res$b.m <- rep(0, n)
    }

    res$ys <- exp(mean(res$x.m) + res$b.m)
    
    return(res) 
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

### real to real transformation
xTransf <- function(x, transf, inverse=FALSE) 
  switch(transf, 
         none=x, 
         sqrt=sqrtR(x, inverse=inverse), 
         log2=logR(x, 2, 2, inverse=inverse), 
         log10=logR(x, 10, 2, inverse=inverse))

axTransfTicks <- function(transf, lim, n=npretty) {
    r <- list(x=pretty(lim, n))
  if (length(r$l)<(0.5*n))
      r$x <- pretty(lim, ceiling(1.5*n))
    r$l <- r$x
    if (transf=='sqrt') {
        r$l <- sqrtR(r$x, inverse = TRUE)
    }
    if (transf=='log10') {
        if (diff(lim)<1.2) {
            r <- list(l=pretty(logR(lim, 10, inverse=TRUE), 10))
            if (length(r$l)<5)
                r <- list(l=pretty(logR(lim, 10, inverse=TRUE), 15))
            r$x <- logR(r$l, 10)
            return(r)
        }
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
                diff(lim), c(0, 0.5, 1, 2, 3, 4, 5))
            lpx0 <- list(
                '1'=c(1.1, 1.2, 1.4, 1.6, 1.8, 
                      2, 2.2, 2.5, 2.8, 3.1, 3.5, 
                      4, 4.5, 5, 5.6, 6.3, 7, 8, 9), 
                '2'=c(1.2, 1.5, 2, 2.5, 3.2, 4, 5, 6.3, 8),
                '3'=c(1.4,1.9,2.6,3.6,5,7), 
                '4'=c(1.7, 3, 5), ##6, 2.5, 3.8, 6),
         '5'=c(2,4), 
         '6'=c(3), 
         '7'=3)
            x0 <- log(
                switch(as.character(b),
                       '1'=lpx0[[2]],
                       '2'=lpx0[[3]], 
                       '3'=lpx0[[4]],
                       '4'=lpx0[[5]],
                       '5'=lpx0[[6]], 
                       '6'=lpx0[[7]], 
                       '7'=lpx0[[7]]), 10)
            nx0 <- length(x0)
            l0 <- floor(lim[1])
            if (l0<0) {
                b <- b - length(l0:(-1))
                r <- list(x=c(rep(l0:(-1), each=nx0)+(1-x0), 
                              rep(0:b, each=nx0) + x0))
            } else {
                r <- list(x=rep(l0+0:b, each=nx0) + x0) 
            }
            r$x <- unique(sort(c(
                floor(lim[1]):ceiling(lim[2]), r$x)))
            r$l <- logR(r$x, 10, inverse=TRUE)
            if (diff(lim)>7) {
                r$l <- logR(pretty(lim, 10), 10, inverse=TRUE)
                if (length(r$l)<7)
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

accMax <- function(x) {
### fix an accumulated serie to
### evitate negative differences
    x.ori <- x
    i.ok <- which(!is.na(x))
    x <- x.ori[i.ok] 
###    k <- 0
    d <- diff(x)
    dn <- which(d<0)
###    print(dn)
    while(length(dn)>0) {
###        k <- k+1
###        cat(k, '\n')
        for (j in dn) {
            a <- x[j] 
            x[j] <- x[j+1]
            x[j+1] <- a
###            print(x)
        }
        d <- diff(x)
        dn <- which(d<0)
###        print(dn)
    }
###    cat('\n')
    x.ori[i.ok] <- x
    return(x.ori) 
}

### select the data for the selected local(s) 
###   prepare it:
### 1. differenced series
### 2. R_t computations 
dataPrepare <- function(slocal) {
    
    ii <- pmatch(slocal, locals)
    
    nl <- length(ii)
    jj <- 7:ncol(wdl[[1]])
    y <- as.matrix(wdl[[1]][ii, jj, drop=FALSE])
    ii0 <- which(colSums(y>0)>0)
    
    if (length(ii0)==0) {
        if (pt) {
            stop(safeError('Sem dados na seleção feita!'))
        } else {
            stop(safeError('No data in the selection made!'))
        }
    }
    
    d <- list(x=vecDate[ii0[1]:ncol(y)])
    d$y <- t(y[, ii0[1]:ncol(y), drop=FALSE])
    d$o <- t(wdl[[2]][ii, jj[ii0[1]:ncol(y)], drop=FALSE])
    
    d$dy <- apply(d$y, 2, function(y) 
        diff(c(0, y)))
    d$do <- apply(d$o, 2, function(y) 
        diff(c(0, y)))
    
    yy <- array(NA, c(dim(d$y), 2))
    yy[,,1] <- d$dy
    yy[,,2] <- d$do
    
    for (l in 1:nl) {
        if (any(yy[, l, 1]<0, na.rm=TRUE)) { 
            yy[, l, 1] <- diff(c(0, accMax(d$y[,l])))
        } 
        if (any(yy[, l, 2]<0, na.rm=TRUE)) { 
            yy[, l, 2] <- diff(c(0, accMax(d$o[,l])))
        } 
    }
    
###tSmoothPoisson <- function(y, X, B, off=rep(0, nrow(X))) 
    w <- weekdays(d$x)
    ww <- model.matrix(~0+w, data.frame(w=w))
    nt <- nrow(ww)
    tk0 <- seq(1, nt, length=round(nt/14))
    bb <- bs(1:nt, knots=tk0)
    bb <- bb[, which(colSums(bb)>0)]
    bb[,2] <- bb[,1] + bb[,2]
    bb[,ncol(bb)-1] <- bb[, ncol(bb)] + bb[, ncol(bb)-1]
    bb <- bb[, 2:(ncol(bb)-1)]

    print(summary(yy[,,1]))
    print(summary(yy[,,2]))
    
    if (mgcv.ok) {
        d$sdy <- apply(yy[,,1, drop=FALSE], 2, SmoothFit, w=w)
        d$sdo <- apply(yy[,,2, drop=FALSE], 2, SmoothFit, w=w)
    } else {
        fSloc <- function(y) {
            if(all(is.na(y))) return(rep(NA, length(y)))
            i1 <- which(ifelse(is.na(y), 0, y)>0)[1]
            if(i1>=length(y)) return(rep(0, length(y)))
            i.ok <- intersect(which(!is.na(y)), (i1+1):length(y))
            r <- y
            bbi <- bb[i.ok, colSums(bb[i.ok, , drop=FALSE])>1]
            xxi <- cbind(bbi, ww[i.ok, , drop=FALSE])
             print(table(is.na(xxi)))
             print(table(is.finite(xxi)))
             print(dim(xxi))
             print(c(n0=length(y), ns=length(i.ok), r=range(i.ok)))
             print(colSums(xxi))
            ff <- glm.fit(xxi, y[i.ok], family=poisson())
            ib <- which(!is.na(ff$coeff[1:ncol(bbi)]))
            ix <- which(!is.na(ff$coeff[ncol(bbi)+1:ncol(ww)]))
            mx <- ww[i.ok, ix, drop=FALSE] %*% ff$coeff[ix+ncol(bbi)]
            r[i.ok] <- exp(drop(bbi[, ib, drop=FALSE] %*% ff$coeff[ib] + mean(mx)))
            return(r * sum(y, na.rm=TRUE)/sum(r, na.rm=TRUE)) 
        }
        d$sdy <- apply(yy[,,1, drop=FALSE], 2, fSloc)
        d$sdo <- apply(yy[,,2, drop=FALSE], 2, fSloc) 
    }
##    print(str(d))
    
    d$yy <- yy
    
    d <- Rtfit(d)
    
    attr(d, 'ii') <- ii 
    
    iiwv <- pmatch(slocal, attr(wvac, 'local'))
    icwb <- pmatch(c('Curitiba(SM), PR - BR',
                     'Curitiba(SMB), PR - BR'), slocal)
    icwb <- icwb[!is.na(icwb)]
    if(length(icwb)>0) {
      iiwv[icwb] <- which(attr(wvac, 'local')=='Curitiba, PR - BR')
    }

    print(c(ny=ncol(y), nv=ncol(wvac[[1]])))
    d$vac <- lapply(wvac, function(m) {
        r <- matrix(NA, length(ii0[1]:ncol(y)), length(slocal))
        ij <- which(!is.na(iiwv))
        if(length(ij)>0) {
          for (l in ij) {
            tmp <- m[iiwv[l], ii0[1]:ncol(m)]
            tmp[which(tmp<1)] <- NA
            r[, l] <- tmp
          }
        }
        return(r)
    })

    d$svac <- lapply(d$vac, function(v) {
      apply(v, 2, fSloc)
    })

    iim <- pmatch(slocal, glocals)
    i2i <- pmatch(glocals[iim], locals[ii])
    
    nl <- length(iim)
    if (nl>0) {
        
##        jj <- (1:ncol(wgmbl[[1]]))[ii0[1]:ncol(y)]
        
        d$gmob <- lapply(wgmbl, function(m)
            t(m[iim, , drop=FALSE]))
        
        if (FALSE) {
            d$sgmob <- lapply(d$gmob, function(m)
                apply(m[, drop=FALSE], 2, SmoothFitG, w=w))
        } else {
            w <- weekdays(vecDate)
            ww <- model.matrix(~0+w, data.frame(w=w))
            nt <- nrow(ww)
            tk0 <- seq(1, nt, length=round(nt/14))
            bb <- bs(1:nt, knots=tk0)
            bb <- bb[, which(colSums(bb)>0)]
            bb[,2] <- bb[,1] + bb[,2]
            bb[,ncol(bb)-1] <- bb[, ncol(bb)] + bb[, ncol(bb)-1]
            bb <- bb[, 2:(ncol(bb)-1)]
            d$sgmob <- lapply(d$gmob, function(m) {
                if (is.null(m)) return(NULL)
                apply(m[, drop=FALSE], 2, function(y) {
                    i <- which(!is.na(y))
                    if(length(i)==0) return(y)
                    if(length(i)<20) return(rep(mean(y, na.rm=TRUE), length(y)))
                    r <- y
                    xxi <- cbind(bb, ww)[i, , drop=FALSE]
                    ff <- glm.fit(xxi, y[i])
                    ib <- which(!is.na(ff$coeff[1:ncol(bb)]))
                    ix <- which(!is.na(ff$coeff[ncol(bb)+1:ncol(ww)]))
                    mx <- ww[i, ix, drop=FALSE] %*% ff$coeff[ix+ncol(bb)]
                    r[i] <- drop(bb[i, ib, drop=FALSE] %*% ff$coeff[ib] + mean(mx))
                    return(r * sum(y, na.rm=TRUE)/sum(r, na.rm=TRUE)) 
                })
            })
        }
        
        attr(d, 'iim') <- iim
        attr(d, 'i2i') <- i2i
        
    }
    
    iam <- lapply(alocals, function(x)
        pmatch(slocal, x))
    i3i <- lapply(1:length(iam), function(k)
        pmatch(alocals[[k]][iam[[k]]], locals[ii]))    
    n3 <- sum(!is.na(unique(unlist(iam))))

    if (n3>0) {

        wD3 <- as.Date(colnames(wambl[[1]]), 'X%Y.%m.%d')
        w <- weekdays(wD3)
        ww <- model.matrix(~0+w, data.frame(w=w))
        nt <- nrow(ww)
        tk0 <- seq(1, nt, length=round(nt/14))
        bb <- bs(1:nt, knots=tk0)
        bb <- bb[, which(colSums(bb)>0)]
        bb[,2] <- bb[,1] + bb[,2]
        bb[,ncol(bb)-1] <- bb[, ncol(bb)] + bb[, ncol(bb)-1]
        bb <- bb[, 2:(ncol(bb)-1)]

        d$amob <- lapply(1:length(iam), function(k) {
            r <- matrix(NA, nrow(ww), length(slocal))
            ia <- iam[[k]]
            nnaa <- which(!is.na(ia))
            if (length(nnaa)>0)
                r[, nnaa] <- t(wambl[[k]][ia[nnaa], , drop=FALSE])
            return(r)
        })

        if (FALSE) {
            d$samob <- lapply(d$amob, function(m) {
                apply(m, 2, SmoothFitG, w=wD3)
            })
        } else {
            d$samob <- lapply(d$amob, function(m) {
                apply(m, 2, function(y) {
                    i <- which(!is.na(y))
                    if(length(i)==0) {
                        r <- rep(NA, length(y))
                    } else {
                        if(length(i)<30) {
                            r <- rep(mean(y, na.rm=TRUE), length(i))
                        }
                        if(length(i)>29) {
                            r <- y
                            xxi <- cbind(bb, ww)[i, , drop=FALSE]
                            ff <- glm.fit(xxi, y[i])
                            ib <- which(!is.na(ff$coeff[1:ncol(bb)]))
                            ix <- which(!is.na(ff$coeff[ncol(bb)+1:ncol(ww)]))
                            mx <- ww[i, ix, drop=FALSE] %*% ff$coeff[ix+ncol(bb)]
                            r[i] <- drop(bb[i, ib, drop=FALSE] %*% ff$coeff[ib] + mean(mx))
                            r <- r * sum(y, na.rm=TRUE)/sum(r, na.rm=TRUE)
                        }
                    }
                    return(r)
                })
            })
        }

        attr(d, 'iam') <- iam
        attr(d, 'i3i') <- i3i
    }
    
    pop <- attr(wdl, 'population')[ii]
    attr(d, 'population') <- pop
    
    return(d) 
    
}

### spline smooth series of non-negative data
SmoothFit <- function(y, w) {
    x0t <- rev(seq(length(y), -7, -7))
    y[y<0] <- NA
    ii <- which(!is.na(y))
    dtmp <- list(tt=ii, r=y[ii], w=w[ii])
    r <- y
    if (length(ii)<10) {
        r[ii] <- mean(dtmp$y)
    } else {
        if (length(ii)<30) {
            sfit <- gam(r ~ 1 + s(tt), poisson(), data=dtmp)
            p.t <- predict(sfit, type='terms')
            r[ii] <- exp(attr(p.t, 'constant') + p.t[,1])
        } else {
            if ((length(levels(dtmp$w))>1) &
                all(table(dtmp$w)>2)) {
                sfit <- gam(r ~ 0 + w + s(tt, m=1), 
                            poisson(), data=dtmp, knots=list(tt=x0t))
                p.t <- predict(sfit, type='terms')
                r[ii] <- exp(p.t[,2] + mean(p.t[,1]))
            } else {
                sfit <- gam(r ~ 1 + s(tt, m=1), poisson(), 
                            data=dtmp, knots=list(tt=x0t))
                p.t <- predict(sfit, type='terms')
                r[ii] <- exp(attr(p.t, 'constant') + p.t[,1])
            }
        }
    }
    return(r)
}

SmoothFitG <- function(y, w) {
    ii <- which((!is.na(y)) & (!is.na(w)))
    if(length(ii)==0) return(y)
    dtmp <- list(tt=ii, r=y[ii], w=factor(w[ii]))
    rr <- y
    if (length(ii)<10) {
        rr[ii] <- mean(dtmp$y)
    } else {
        if (length(ii)<30) {
            sfit <- gam(r ~ 1 + s(tt), data=dtmp)
            p.t <- predict(sfit, type='terms')
            rr[ii] <- (attr(p.t, 'constant') + p.t[,1])
        } else {
            if ((length(levels(dtmp$w))>1) &
                all(table(dtmp$w)>2)) {
                rii <- range(ii, na.rm=TRUE)
                kii <- seq(rii[1], rii[2], length=round(diff(rii)/14))
                bb <- bs(ii, knots=kii)
                bb <- bb[, which(colSums(bb)>0)]
                bb[,2] <- bb[,1] + bb[,2]
                bb[,ncol(bb)-1] <- bb[, ncol(bb)] + bb[, ncol(bb)-1]
                dtmp$b <- bb[, 2:(ncol(bb)-1)]
                sfit <- lm(r ~ 0 + w + b, data=dtmp)
                dw <- dtmp
                dw$b <- dtmp$b*0 
                p.x <- predict(sfit, newdata=dw)
                p.t <- predict(sfit) - p.x + mean(p.x)
                ##              if (nrow(p.t)==length(ii)) {
                ##rr[ii] <- (p.t[,2] + mean(p.t[,1]))
                rr[ii] <- p.t
                ##            } else {
                ##            rr[ii] <- mean(dtmp$y)
                ##          }
          } else {
              sfit <- gam(r ~ 1 + s(tt), data=dtmp)
              p.t <- predict(sfit, type='terms')
              rr[ii] <- (attr(p.t, 'constant') + p.t[,1])
          }
        }
    }
    return(rr)
}

### do the R_t computations 
Rtfit <- function(d, a=0.5, b=1) {
    
    x0t <- rev(seq(nrow(d$yy), -7, -7))
    
    pw <- pgamma(0:21, shape=(5.8/4)^2, scale=4^2/5.8)[1:15]
    w <- diff(pw)/sum(diff(pw))
    n0 <- length(w)
    n1 <- nrow(d$sdy)
    nl <- ncol(d$sdo)
    
    yy <- ys <- d$yy
    yy[,,1] <- d$yy[,,1]
    yy[,,2] <- d$yy[,,2]
    ys[,,1] <- d$sdy
    ys[,,2] <- d$sdo
    
    d$ee <- array(0, c(n0 + n1, nl, 2))
    d$Rtupp <- d$Rtlow <- d$Rt <- array(NA, c(n1, nl, 2)) 
    
    for (k in 1:2) {
        for (l in 1:nl) {
            y0 <- ys[, l, k]
            i1 <- which(y0>0)[1]
            ii <- which(!is.na(y0)) 
            ii <- ii[ii>=i1]
            ii0 <- 1:max(n0, i1+14)
            d$ee[ii0, l, k] <- max(1, mean(y0[ii0], na.rm=TRUE))
            for (i in ii) 
                d$ee[i+1:n0, l, k] <- 
                    d$ee[i+1:n0, l, k] + y0[i] * w 
            d$ee[d$ee<0.01] <- 0.01
            if ((length(ii)>19) & (mgcv.ok)) {
                fRt <- gam(y ~ 0 + w + s(x, m=1), poisson(),
                           knots=list(x=x0t), 
                           data=list(
                               w = factor(weekdays(d$x[ii])), 
                               x = ii, y = yy[ii,l,k]), 
                           offset = log(d$ee[ii,l,k]))
                tpred <- predict(fRt, type='terms', se=TRUE)
                ytmp <- exp(tpred$fit[, 2] + 
                            mean(tpred$fit[,1]))*d$ee[ii,l,k]
            } else {
                ytmp <- ys[ii, l, k]
            }
            ytmp[union(1:n0, which(yy[ii, l, k]<0))] <- NA
            d$Rt[ii, l, k] <- (ytmp + a)/(d$ee[ii,l,k] + b)
### IC consider gamma(a+y, b+E) 
            d$Rtlow[ii,l,k] <- qgamma(
                0.025, ytmp+a, d$ee[ii,l,k]+b)
            d$Rtupp[ii,l,k] <- qgamma(
                0.975, ytmp+a, d$ee[ii,l,k]+b)
        }
    }

    return(d) 
    
}

## display the data and R_t
data2plot <- function(d,
                      popDivide,
                      variables,
                      dateRange, 
                      plots,
                      showPoints,
                      transf, 
                      legpos) {
    
    if ((length(plots)==0) |
        (any(plots%in%(1:4)) & (length(variables)<1))) {
        if (pt) {
            stop(safeError(
                'Favor selecionar pelo menos uma variável!'))
        } else {
            stop(safeError(
                'Please select at least one variable!'))
        }
    }
    
    v <- pmatch(
        variables, 
        c('cases', 'deaths'))
    
    plots <- pmatch(plots, allpls)
    
    wplot <- integer(7)
    if (any(plots==1))
        wplot[1] <- 1
    if (any(plots==2))
        wplot[2] <- 2
    if (any(plots==3))
        wplot[3] <- 3
    if (any(plots==4))
        wplot[4] <- 4
    if (any(plots%in%c(5,6)))
        wplot[5] <- 5
    if (any((plots>6) & (plots<13)))
        wplot[6] <- 6
    if (any(plots>12))
        wplot[7] <- 7
    wplot <- wplot[wplot>0]
    nplot <- length(wplot)

    iplot <- 0
    
    mgpp <- c(2.5, 0.5, 0)
    if (popDivide)
        mgpp[1] <- 2
    if(nplot>6) {
      nrwplot <- ncwplot <- 3
    } else {
      if (nplot>4) {
        nrwplot <- 3
        ncwplot <- 2
      } else {
        if (nplot==4) {
          ncwplot <- nrwplot <- 2
        } else {
          nrwplot <- nplot
          ncwplot <- 1
        }
      }
    }

    mmar <- c(0.5, 4.5, 0.5, 0.5)
    par(mfrow=c(nrwplot, ncwplot), 
        mar=mmar, mgp=mgpp)
    
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
    nl <- ncol(d$y) 
    if (nl>100) {
        if (pt) {
            stop(safeError('Muitos lugares selecionados!'))
        } else {
            stop(safeError('Too many places!'))
        }
    }
    
    if (popDivide) {
        popd <- attr(d, 'population')*1e-6
    } else {
        popd <- attr(d, 'population')*0 + 1
    }
    
    for (j in 1:4) {
        d[[length(d)+1]] <- d[[3+j]]
        for (l in 1:nl) {
            d[[length(d)]][, l] <- xTransf(
                d[[3+j]][, l]/popd[l], transf)
        }
    }
    
    names(d)[(nd0+1):length(d)] <-
        paste0(names(d)[4:7], '.plot')

    jj0 <- which((d$x>=sxlm[1]) & 
                 (d$x<=sxlm[2]))
    if (length(jj0)==0) {
        if (pt) {
            stop(safeError('Sem dados na seleção!'))
        } else {
            stop(safeError('No data in the selection!'))
        }
    }
    
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
    
    xlm <- xlm0 <- range(d$x[jj])
    leg.cex <- 1 - 0.7 * sqrt(nl)/10
    if (legpos=='right') {
        leg.ncols <- ceiling(sqrt(nl)/3)
        xlm[2] <- xlm0[2] + diff(xlm0)*(0.2+log(leg.ncols,2)/10)
        y.ex2 <- y.ex1 <- 0.00
    } else {
        legpos <- 'topleft'
        leg.ncols <- ceiling(sqrt(nl)*2)
        y.ex1 <- log(max(nnll) + 2, 3) * 
            ceiling(nl/leg.ncols) * 0.25 * leg.cex
        y.ex2 <- length(v) * 
            log(ceiling(nl/leg.ncols)+1, 2) * 0.125 * leg.cex
    }
    xl <- list(x=pretty(xlm0, 10))
    if (length(xl$x)<10)
        xl <- list(x=pretty(xlm0, 15))
    xl$l <- format(xl$x, '%b,%d')
    xl$x <- xl$x[which(xl$x<=(xlm0[2] + 1))]
    
    if (pt) {
        ylabs <- list(c(
            'Contagem diária',
            'Casos confirmados por dia',
            'Óbitos confirmados por dia'),
            c('Contagem acumulada',
              'Casos confirmados acumulados',
              'Óbitos confirmados acumulados'), 
            'Número de reprodução\n(infectados por infectante)', 
            'Taxa de letalidade (%)')
        if (popDivide) {
            ylabs[[1]][1:3] <- paste0(ylabs[[1]][1:3], '\npor 1M habitantes')
            ylabs[[2]][1:3] <- paste0(ylabs[[2]][1:3], '\npor 1M habitantes')
        }
    } else {
        ylabs <- list(
            c('Daily counts',
              'Daily number of cases',
              'Daily number of deaths'),
            paste('Accumulated',
                  c('count', 'of cases', 'of deaths')), 
            'Reproduction number\n(infecteds per infectee)', 
            'Fatality rate (%)')
        if (popDivide) {
            ylabs[[1]][1:3] <- paste0(ylabs[[1]][1:3], '\nper 1M inhabitants')
            ylabs[[2]][1:3] <- paste0(ylabs[[2]][1:3], '\nper 1M inhabitants')
        }
    }
    
    nl <- ncol(d$y)
    if (nl==1) {
        scol <- rgb(.1,.1,.1,.7)
        shad.col <- rgb(.5,.5,.5,.35)
    } else {
        ucol <- 0:(nl-1)/(nl-1)
        gcol <- 1-2*abs(ucol-mean(ucol))
        scol <- rgb(ucol, gcol, 1-ucol, 0.9)
        shad.col <- rgb(0.3+0.7*ucol, 
                        0.3+0.7*gcol, 
                        0.3+0.7*(1-ucol), 0.35)
    }
    
    getNc <- function(x) {
        x <- c(0, x) 
        for (j in 2:length(x))
            if (is.na(x[j]))
                x[j] <- x[j-1]
        x[jj[length(jj)]+1] - x[jj[1]]
    }
    nn1 <- apply(d$y, 2, getNc)
    nn2 <- apply(d$o, 2, getNc)
    
    if (any(v==2)) {
        oloc <- order(
            nn2, decreasing = TRUE)
    } else {
        oloc <- order(
            nn1, decreasing = TRUE)
    }
    
    if (any(plots==1)) {
        iplot <- iplot + 1 
        if (nrwplot==1) 
            par(mar=c(2, 4.5, 0, 0.5))
        
        y.ex1 <- y.ex1*ifelse(any(plots==2), 0.33, 1)
        
        if (length(v)==2) {
            
            if (showPoints) {
                ylm <- range(d$dy.plot[jj,], 
                             d$do.plot[jj,], na.rm=TRUE)
            } else {
                ylm <- range(d$sdy.plot[jj,],
                             d$sdo.plot[jj,], na.rm=TRUE)
            }
            
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
                if (showPoints) {
                    ylm <- range(
                        d$dy.plot[jj, ], na.rm=TRUE)
                } else {
                    ylm <- range(d$sdy.plot[jj, ], na.rm=TRUE)
                }
                
                plot(d$x, 
                     d$dy.plot[,1], 
                     axes=FALSE, 
                     xlim=xlm,
                     ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
                     type = 'n', 
                     xlab='', 
                     ylab=ylabs[[1]][2])
                
            } else {
                
                if (showPoints) {
                    ylm <- range(d$do.plot[jj, ], na.rm=TRUE)
                } else {
                    ylm <- range(d$sdo.plot[jj, ], na.rm=TRUE)
                }
                
                plot(d$x, 
                     d$do.plot[,1], 
                     axes=FALSE, 
                     xlim=xlm,
                     ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1*length(wplot)), 
                     type = 'n', 
                     xlab='', 
                     ylab=ylabs[[1]][3])
                
            }
        }
        
        yl <- axTransfTicks(transf, ylm)
        yab <- par()$usr[3:4]
        i.yl <- which(findInterval(
            yl$x, ylm+c(-1,1)*0.1*diff(ylm))==1)
        
        axis(2, yl$x[i.yl], round(yl$l[i.yl]), las=1)
        segments(rep(xlm0[1], length(i.yl)), yl$x[i.yl],
                 rep(xlm0[2], length(i.yl)), yl$x[i.yl],
                 lty=2, col=gray(0.7, 0.5))
        
        if (any(v==1)) {
            for (l in 1:nl) {
                lines(d$x, d$sdy.plot[,l], col=scol[l], lwd=2)
                ##            lines(d$x, d$ee.plot[,l,1], col=4, lty=2)
                if (showPoints)
                    points(d$x, d$dy.plot[,l], 
                           cex=1-log(nl,10)/2, pch=19, col=scol[l])
            }        
        }
        if (any(v==2)) {
            for (l in 1:nl) {
                lines(d$x, d$sdo.plot[,l], col=scol[l], 
                      lty=length(v), lwd=3)
                ##          lines(d$x, d$ee.plot[,l,2], col=4, lty=2)
                if (showPoints)
                    points(d$x, d$do.plot[,l], 
                           cex=1-log(nl,10)/2, pch=8, col=scol[l])
            }
        }
        
        if (any(plots==2)) {
            if (any(v==1)) {
                if (pt) {
                    legend(legpos, c('Casos', 'Óbitos')[v],
                           lwd=2, lty=v, bty='n', title='Variável')
                } else {
                    legend(legpos, c('Cases', 'Deaths')[v],
                           lwd=2, lty=v, bty='n', title='Variable')
                }
            } else {
                if (pt) {
                    legend(legpos, c('Óbitos'),
                           lwd=2, lty=1, bty='n', title='Variável')
                } else {
                    legend(legpos, c('Deaths'),
                           lwd=2, lty=1, bty='n', title='Variable')
                }
            }
        } else {
            nn1x <- format(nn1, big.mark = ',')
            nn2x <- format(nn2, big.mark = ',')
            if (pt) {
                nn1x <- gsub(',', '.', nn1x, fixed=TRUE)
                nn2x <- gsub(',', '.', nn2x, fixed=TRUE)
            }
            if (length(v)==2) {
                if (nl>1) {
                    if (pt) {
                        nlab <- paste0(nn1x, ' C, ', nn2x, ' Ó')
                    } else {
                        nlab <- paste0(nn1x, ' C, ', nn2x, ' D')
                    }
                } else {
                    nlab <- c(nn1x, nn2x)
                }
            } else {
                if (v==1) {
                    nlab <- nn1x 
                } else {
                    nlab <- nn2x 
                }
            }
            if (nl>1) {
                legend(legpos, paste0(lll, '\n', nlab)[oloc], 
                       ##inset = c(0, -0.05),
                       col=scol[oloc], lty=1, lwd=5,
                       bty='n', xpd=TRUE,
                       y.intersp=sqrt(0.5+max(nnll)),
                       cex=leg.cex, ncol=leg.ncols)
            } else {
                if (pt) {
                    llg <- paste(c('Casos', 'Óbitos'), ':', nlab)
                } else {
                    llg <- paste(c('Cases', 'Deaths'), ':', nlab)
                }
                if (showPoints) {
                    legend(legpos, llg[v], bty='n', 
                           pch=c(19,8)[v], lty=v, lwd=2)
                } else {
                    legend(legpos, llg[v], bty='n', 
                           lty=1:length(v), lwd=2)
                }
        }
        }
        if ((nplot-iplot)<ncwplot)
            axis(1, xl$x, format(xl$x, '%b,%d'))

        ##    abline(v=xl$x, col=gray(0.5, 0.5), lty=2)
        segments(xl$x, rep(ylm[1], length(xl$x)),
                 xl$x, rep(ylm[2], length(xl$x)),
                 col=gray(0.5, 0.5), lty=2)
    }
    
    par(mgp=mgpp)
    
    if (any(plots==2)) {
        iplot <- iplot + 1        
        
        y.ex1 <- y.ex1*ifelse(any(plots==1), 3, 1)
        if ((nplot-iplot)<ncwplot)
            par(mar=c(2, 4.5, 0, 0.5))
        
        d$y.plot <- d$y
        d$o.plot <- d$o
        d$sy.plot <- d$sy <- apply(d$sdy, 2, cumsum)
        d$so.plot <- d$so <- apply(d$sdo, 2, cumsum)
        for (l in 1:nl) {
            d$y.plot[, l] <- xTransf(
                d$y[, l]/popd[l], transf)
            d$o.plot[, l] <- xTransf(
                d$o[, l]/popd[l], transf)
            d$sy.plot[, l] <- xTransf(
                d$sy[, l]/popd[l], transf)
            d$so.plot[, l] <- xTransf(
                d$so[, l]/popd[l], transf)
        }
        
        if (length(v)==2) {
            
            if (showPoints) {
                ylm <- range(d$y.plot[jj,], 
                             d$o.plot[jj,], na.rm=TRUE)
            } else {
                ylm <- range(d$sy.plot[jj,],
                     d$so.plot[jj,], na.rm=TRUE)
            }
            
            plot(d$x, 
                 d$y.plot[,1], 
                 axes=FALSE,
                 xlim=xlm, 
                 ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
                 type = 'n',
                 xlab='',
                 ylab=ylabs[[2]][1]) 
            
        } else {
            
            if (v==1) {
                if (showPoints) {
                    ylm <- range(
                        d$y.plot[jj, ], na.rm=TRUE)
                } else {
                    ylm <- range(d$sy.plot[jj, ], na.rm=TRUE)
                }
                
                plot(d$x, 
                     d$y.plot[,1], 
                     axes=FALSE, 
                     xlim=xlm,
                     ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1), 
                     type = 'n', 
                     xlab='', 
                     ylab=ylabs[[2]][2])
                
            } else {
                
                if (showPoints) {
                    ylm <- range(d$o.plot[jj, ], na.rm=TRUE)
                } else {
                    ylm <- range(d$so.plot[jj, ], na.rm=TRUE)
                }
                
                plot(d$x, 
                     d$do.plot[,1], 
                     axes=FALSE, 
                     xlim=xlm,
                     ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex1*length(wplot)), 
                     type = 'n', 
                     xlab='', 
                     ylab=ylabs[[2]][3])
                
            }
        }
        
        yl <- axTransfTicks(transf, ylm)
        yab <- par()$usr[3:4]
        i.yl <- which(findInterval(
            yl$x, ylm+c(-1,1)*0.1*diff(ylm))==1)
        
        axis(2, yl$x[i.yl], round(yl$l[i.yl]), las=1)
        segments(rep(xlm0[1], length(i.yl)), yl$x[i.yl],
                 rep(xlm0[2], length(i.yl)), yl$x[i.yl],
                 lty=2, col=gray(0.7, 0.5))
        
        if (any(v==1)) {
            for (l in 1:nl) {
                lines(d$x, d$sy.plot[,l], col=scol[l], lwd=2)
                if (showPoints)
                    points(d$x, d$y.plot[,l], 
                           cex=1-log(nl,10)/2, pch=19, col=scol[l])
            }        
        }
        if (any(v==2)) {
            for (l in 1:nl) {
                lines(d$x, d$so.plot[,l], col=scol[l], 
                      lty=length(v), lwd=3)
                if (showPoints)
                    points(d$x, d$o.plot[,l], 
                           cex=1-log(nl,10)/2, pch=8, col=scol[l])
            }
        }
        
        nn1x <- format(nn1, big.mark = ',')
        nn2x <- format(nn2, big.mark = ',')
        if (pt) {
            nn1x <- gsub(',', '.', nn1x, fixed=TRUE)
            nn2x <- gsub(',', '.', nn2x, fixed=TRUE)
        }
        if (length(v)==2) {
            if (nl>1) {
                if (pt) {
                    nlab <- paste0(nn1x, ' C, ', nn2x, ' Ó')
                } else {
                    nlab <- paste0(nn1x, ' C, ', nn2x, ' D')
                }
            } else {
                nlab <- c(nn1x, nn2x)
            }
        } else {
            if (v==1) {
                nlab <- nn1x 
            } else {
                nlab <- nn2x 
            }
        }
        if (nl>1) {
            legend(legpos, paste0(lll, '\n', nlab)[oloc], 
                   ##inset = c(0, -0.05),
                   col=scol[oloc], lty=1, lwd=5,
                   bty='n', xpd=TRUE,
                   y.intersp=sqrt(0.5+max(nnll)),
                   cex=leg.cex, ncol=leg.ncols)
        } else {
            if (pt) {
                llg <- paste(c('Casos', 'Óbitos'), ':', nlab)
            } else {
                llg <- paste(c('Cases', 'Deaths'), ':', nlab)
            }
            if (showPoints) {
                legend(legpos, llg[v], bty='n', 
                       pch=c(19,8)[v], lty=v, lwd=2)
            } else {
                legend(legpos, llg[v], bty='n', 
                       lty=1:length(v), lwd=2)
            }
        }

        if ((nplot-iplot)<ncwplot)
            axis(1, xl$x, format(xl$x, '%b,%d'))

    }
    
    par(mar=mmar)
    if (any(plots==3)) {
        iplot <- iplot + 1
        if ((nplot-iplot)<ncwplot) 
            par(mar=c(2, 4.5, 0, 0.5), mgp=c(2,0.5,0))
        
        ylm <- range(0.91, 1.1, d$Rtlow[jj,,v], 
                     d$Rtupp[jj,,v], na.rm=TRUE)
        if (showPoints) {
            rtobs <- array(NA, c(nrow(d$dy), nl, 2))
            rtobs[,,1] <- d$dy[,]/d$ee[1:nrow(d$do),,1]
            rtobs[,,2] <- d$do[,]/d$ee[1:nrow(d$do),,2]
            ylm <- range(1, rtobs[jj, , v], na.rm=TRUE)
        }
        
        plot(d$x,
             xTransf(d$Rt[,1,1], transf), 
             xlim=xlm,
             ylim=xTransf(
                 c(ylm[1], ylm[2]+diff(ylm)*y.ex2*length(wplot)),
                 transf), 
             type='n', xlab='', las=1,
             ylab=ylabs[[3]], 
             axes=FALSE)
        
        for (k in v) {
            for (l in 1:nl) {
                if (showPoints) 
                    points(d$x,
                           xTransf(rtobs[, l, k], transf), 
                           pch=c(19,8)[k], col=scol[l])
                ii <- which(!is.na(d$Rt[, l, k]))
                iid <- which(diff(ii)>1)
                s1 <- c(1, iid+1)
                s2 <- c(iid, length(ii))
                for (s in 1:length(s1)) {
                    idx <- ii[s1[s]:s2[s]]
                    polygon(c(d$x[idx],
                              rev(d$x[idx]),
                              d$x[idx[1]]), 
                            xTransf(c(d$Rtlow[idx,l,k],
                                      rev(d$Rtupp[idx,l,k]),
                                      d$Rtlow[idx[1],l,k]), transf), 
                            col=shad.col[l], border=shad.col[l])
                }
                lines(d$x,
                      xTransf(d$Rt[, l, k], transf),
                      col=scol[l], lwd=2, lty=k)
            }
        }
        
        ylr <- axTransfTicks(transf, xTransf(ylm, transf))
        axis(2, ylr$x, ylr$l, las=1) 
        segments(rep(xlm0[1], length(ylr)),
                 ylr$x, 
                 rep(xlm0[2], length(ylr)),
                 ylr$x, 
                 lty=2, col=gray(0.7, 0.5))
        segments(xlm0[1], xTransf(1, transf),
                 xlm0[2], xTransf(1, transf))
###    abline(h=par()$usr[4])
        
        rtu.last <- rtl.last <- rt.last <- matrix(NA, nl, 2)
        for (k in 1:2) {
            for (l in 1:nl) {
                il <- jj[tail(which(!is.na(d$Rt[jj,l,k])),1)]
                if (length(il)>0) {
                    rt.last[l, k] <- d$Rt[il, l, k, drop=FALSE]
                    rtl.last[l, k] <- d$Rtlow[il, l, k, drop=FALSE]
                    rtu.last[l, k] <- d$Rtupp[il, l, k, drop=FALSE]
                }
            }
        }
        
        llr <- paste0(
            sprintf('%1.2f', rt.last[, v[1]]), '(',
            sprintf('%1.2f', rtl.last[, v[1]]), ', ',
            sprintf('%1.2f', rtu.last[, v[1]]), ')')[oloc]
        if (length(v)==2) {
            llrd <- paste0(sprintf('%1.2f', rt.last[, v[2]]), '(',
                           sprintf('%1.2f', rtl.last[, v[2]]), ', ',
                           sprintf('%1.2f', rtu.last[, v[2]]), ')'
                           )[oloc]
            if (any(plots==1)) {
                llr <- c(llr, llrd) 
                iill <- rep(1:nl, each=length(v)) + 
                    rep(c(0, nl), nl)
                llr <- llr[iill]
            } else {
                if (pt) {
                    llr <- paste0('C: ', llr, '\nÓ: ', llrd)
                } else {
                    llr <- paste0('C: ', llr, '\nD: ', llrd)
                }
            }
        }
        if (pt) {
            llr <- gsub('.', ',', 
                        gsub(',', ';', llr, 
                             fixed=TRUE), fixed=TRUE)
        }
        if (!(any(plots==1))) {
            llr <- paste0(lll[oloc], '\n', llr)
        }
        if (any(plots==1)) {
            legend(legpos, llr, 
                   col=rep(scol[oloc], each=length(v)), 
                   lty=rep(1:length(v), nl), lwd=2,
                   bty='n', xpd=TRUE, cex=leg.cex*1.2, 
                   ncol=leg.ncols)
        } else {
            legend(legpos, llr, 
                   col=scol[oloc],  
                   lty=1, lwd=2,
                   bty='n', xpd=TRUE, cex=leg.cex*1.2, 
                   ncol=leg.ncols)
        }
        
        if ((nplot-iplot)<ncwplot) 
            axis(1, xl$x, format(xl$x, '%b,%d'))
        
        ##  abline(v=xl$x, col=gray(0.5, 0.5), lty=2)
        segments(xl$x, rep(ylm[1], length(xl$x)),
                 xl$x, rep(ylm[2], length(xl$x)),
                 col=gray(0.5, 0.5), lty=2)
    }    
    
    par(mar=mmar) 
    if (any(plots==4)) {
        iplot <- iplot + 1
        if ((nplot-iplot)<ncwplot) 
            par(mar=c(2, 4.5, 0, 0.5))
        
        arate <- 100*d$o/d$y
        arate[d$y<1] <- NA
        rate <- 100*d$do/d$dy
        rate[(d$dy<1) | (rate<0)] <- NA 
        srate <- 100*d$sdo/d$sdy
        srate[(d$sdy<1) | (srate<0)] <- NA
        
        for (l in 1:ncol(arate)) {
            rate[, l] <- xTransf(rate[, l], transf)
            arate[, l] <- xTransf(arate[, l], transf)
            srate[, l] <- xTransf(srate[, l], transf)
        }    
        
        ylm <- range(srate[jj,], arate[jj,], na.rm=TRUE)
        if (showPoints)
            ylm <- range(rate[jj,], na.rm=TRUE)
        if (transf=='none')
            if (ylm[1]<0.1)
                ylm[1] <- 0.1
        
        plot(d$x,
             rate[,1], 
             xlim=xlm,
             ylim=c(ylm[1], ylm[2]+diff(ylm)*y.ex2*length(wplot)), 
             type='n', xlab='',
             ylab=ylabs[[4]], 
             axes=FALSE)
        
        for (l in 1:nl) {
            lines(d$x, srate[,l], col=scol[l], lwd=2)
            lines(d$x, arate[,l], lty=2, col=scol[l], lwd=2)
            if (showPoints)
                points(d$x, rate[,l], 
                       cex=1-log(nl,10)/2, pch=19, col=scol[l])
        }
        
        if ((nplot-iplot)<ncwplot) 
            axis(1, xl$x, format(xl$x, '%b,%d'))
        
        if(transf=='none'){
            axis(2,pretty(par()$usr[3:4],n=10),las=1)
            ##      abline(h=pretty(par()$usr[3:4],n=20),lty=2)
            yy00 <- pretty(par()$usr[3:4], n=20)
            segments(rep(d$x[1], length(yy00)), yy00,
                     rep(d$x[length(d$x)], length(yy00)),
                     lty=2, col=gray(0.5, 0.5))
        } else{
            y0l <- c(0, c(1, 3, 5), c(1, 2, 4)*10) 
            axis(2, xTransf(y0l, transf), y0l, las=1) 
            ##    abline(h=xTransf(y0l, transf),
            ##          lty=2, col=gray(0.5,0.5))
            segments(rep(d$x[1], length(y0l)), y0l,
                     rep(d$x[length(d$x)], length(y0l)), y0l,
                     lty=2, col=gray(0.5, 0.5))
        }
        
        if (pt) {
            lleg3 <- c('Diária', 'Acumulada')
        } else {
            lleg3 <- c('Daily', 'Accumulated')
        }
        if (any(plots%in%c(1,2,3))) {
            legend(legpos, lleg3, lty=1:2, lwd=c(2,1), 
                   ncol=2-(legpos=='right'), bty='n')
        } else {
            legend(legpos, lll[oloc], lty=1, lwd=c(2),
                   col=scol[oloc], ncol=leg.ncols, bty='n')
        }
        ##    abline(v=xl$x, col=gray(0.5, 0.5), lty=2)
##        segments(xl$x, rep(ylm[1], length(xl$x)),
  ##               xl$x, rep(ylm[2], length(xl$x)),
    ##             col=gray(0.5, 0.5), lty=2)
        
    }

    if(any(plots%in%c(5,6))) {
      iplot <- iplot + 1
      if ((nplot-iplot)<ncwplot) 
        par(mar=c(2, 4.5, 0, 0.5))
      jjvac <- which((5:6) %in% plots)
      Pop <- attr(d, 'population')
      for(k in 1:2) {
        for(l in 1:ncol(d$vac[[k]])) {
          tmp <- d$vac[[k]][,l]
          tmp[is.na(tmp)] <- 0
          d$vac[[k]][,l] <- cumsum(tmp)
          tmp <- d$svac[[k]][,l]
          tmp[is.na(tmp)] <- 0
          d$svac[[k]][,l] <- cumsum(tmp) 
          if(popDivide) {
            d$vac[[k]][,l] <- 100*d$vac[[k]][,l]/Pop[l]
            d$svac[[k]][,l] <- 100*d$svac[[k]][,l]/Pop[l]
          }
        }
      }
      ylm <- range(unlist(d$svac[jjvac]), na.rm=TRUE)
      if(showPoints)
        ylm <- range(ylm, unlist(d$vac[jjvac]), na.rm = TRUE)
      plot(d$x, 
           rowMeans(d$vac[[1]], na.rm=TRUE), 
           xlim=xlm, ylim=ylm, type='n', axes=F,
           xlab='', ylab=ifelse(popDivide, "% doses/Pop", '# doses'))
      axis(2)
      if ((nplot-iplot)<ncwplot) 
        axis(1, xl$x, format(xl$x, '%b,%d'))
      for(k in jjvac) {  
        for(j in 1:ncol(d$vac[[k]])) {
          lines(d$x, d$svac[[k]][,j], col=scol[j], lty=k, lwd=3)
          if(showPoints)
            points(d$x, d$vac[[k]][,j], col=scol[j], pch=k)
        }
      }
      if (showPoints) {
        legend(legpos, names(d$vac)[jjvac], pch=jjvac, lty=jjvac, lwd=3, bty='n')
      } else {
        legend(legpos, names(d$vac)[jjvac], lty=jjvac, lwd=3, bty='n') 
      }
      segments(xl$x, rep(ylm[1], length(xl$x)),
               xl$x, rep(ylm[2], length(xl$x)),
               col=gray(0.5, 0.5), lty=2)
      yllv <- pretty(par()$usr[3:4], 10)
      segments(rep(xlm0[1], length(yllv)), yllv, 
               rep(xlm0[2], length(yllv)), yllv,
               col=gray(0.5, 0.5), lty=2)
    }

    if (any((plots>6) & (plots<13))) {
        
        iplot <- iplot + 1
        
        if ((nplot-iplot)<ncwplot) 
            par(mar=c(2, 4.5, 0, 0.5))
        
        i2i <- attr(d, 'i2i')
        
        if (length(i2i)>0) {
            jjp <- plots[(plots>6) & (plots<13)]-6
            
            if (length(jjp)>0) {

                jj.g <- which((vecDate>=d$x[jj[1]]) &
                              (vecDate<=d$x[jj[length(jj)]])) 
                ylm2 <- range(c(unlist(lapply(
                    d$sgmob[jjp], function(m) m[jj.g,])),
                    -5, 5), na.rm=TRUE)
             
                if (showPoints) 
                    ylm2 <- range(c(unlist(lapply(
                        d$gmob[jjp], function(m) m[jj.g,])), 
                        -5, 5, ylm2), na.rm=TRUE)
             
                if (all(is.finite(ylm2))) {
                    plot(d$x, ##d$mob[[1]][,1],
                         type='n', axes=FALSE,
                         xlim=xlm, ylim=ylm2,
                         ylab=paste(ylmob,'(Google)'))
                } else {
                    plot(d$x,
                         xlim=xlm, ylim=c(-100,100),
                         type='n', axes=FALSE,
                         ylab=paste(ylmob,'(Google)'))
                }
                
                jjl <- 1:length(jjp)
                if (length(jjl)>4) {
                    jlty <- rep(1:3, 2)[jjl]
                    jlwd <- rep(1:2, each=3)[jjl]
                } else {
                    if (length(jjl)>2) {
                        jlty <- rep(1:2, 2)[jjl]
                        jlwd <- rep(1:2, each=2)[jjl]
                    } else {
                        jlty <- 1:2
                        jlwd <- c(2,2)
                    }
                }
                jlwd <- 2*jlwd
                for (l in 1:length(i2i)) { ##ncol(d$mob[[1]])) {
                    for (j in jjl) {
                        if (showPoints)
                            points(vecDate[jj.g], d$gmob[[jjp[j]]][jj.g, l],
                                   pch=jjp[j], col=scol[i2i[l]])
                        lines(vecDate[jj.g], d$sgmob[[jjp[j]]][jj.g, l],
                              lty=jlty[j], lwd=jlwd[j],
                              col=scol[i2i[l]])
                    }
                }
            } else {
                jlwd <- jlty <- jjl <- NULL
            }
            
            if (any(plots%in%c(1,2,3,4))) {
                if (showPoints) {
                    legend(legpos, allpls[-(1:6)][jjp],
                           pch=jjp, lty=jlty, lwd=jlwd, bty='n')
                } else {
                    legend(legpos, allpls[-(1:6)][jjp], 
                           lty=jlty, lwd=jlwd, bty='n')
                }
            } else {
                if (showPoints) {
                    legend(legpos, c(lll[oloc], allpls[-(1:6)][jjp]),
                           pch=c(rep(1, length(oloc)), jjp), 
                           lty=c(rep(1, length(oloc)), jlty),
                           lwd=c(rep(2, length(oloc)), jlwd),
                           col=c(scol[oloc], rep(1, length(jjp))), 
                           ncol=leg.ncols, bty='n')
                } else {
                    legend(legpos, c(lll[oloc], allpls[-(1:6)][jjp]),
                           lty=c(rep(1, length(oloc)), jlty),
                           lwd=c(rep(2, length(oloc)), jlwd),
                           col=c(scol[oloc], rep(1, length(jjp))), 
                           ncol=leg.ncols, bty='n')
                }
            }            
            
            if ((nplot-iplot)<ncwplot) 
                axis(1, xl$x, format(xl$x, '%b,%d'))
            
            axis(2, las=1)

            segments(xl$x, rep(ylm2[1], length(xl$x)),
                     xl$x, rep(ylm2[2], length(xl$x)),
                     col=gray(0.5, 0.5), lty=2)
            yy00 <- pretty(ylm2, 10) 
            segments(rep(xlm0[1], length(yy00)), yy00,
                     rep(xlm0[2], length(yy00)), yy00, 
                     lty=2, col=gray(0.5, 0.5))    
            abline(h=0)
            
        }
    }
    
    if (any(plots>12)) {
        par(mar=c(2, 4.5, 0, 0.5))
        iplot <- iplot + 1
        i3i <- attr(d, 'i3i')
        jjp2 <- plots[(plots>12)]-12
        jjl2 <- 1:length(jjp2)
        
        if (length(jjp2)>0) {
            
          wD3 <- as.Date(colnames(wambl[[1]]), 'X%Y.%m.%d')
          jj3 <- which((wD3>=d$x[jj[1]]) & (wD3<=d$x[jj[length(jj)]]))
          
          ylm3 <- range(c(unlist(lapply(
                d$samob[jjp2], function(m) m[jj3, ])), 
                80, 125), na.rm=TRUE)
          if (showPoints) {
                ylm3 <- range(c(unlist(lapply(
                    d$amob[jjp2], function(m) m[jj3,])),
                    ylm3, 80, 125), na.rm=TRUE)
            } 
          
            if (all(is.finite(ylm3))) {                
                plot(attr(wambl[[1]], 'Date'), ##d$mob[[1]][,1],
                     type='n', axes=FALSE,
                     xlim=xlm, ylim=ylm3,
                     ylab=paste(ylmob,'(Apple)'))
            } else {
                plot(attr(wambl[[1]], 'Date'),
                     xlim=xlm, ylim=c(0,200),
                     type='n', axes=FALSE,
                     ylab=paste(ylmob,'(Apple)'))
            }
            
            if (length(jjl2)>2) {
                jlty2 <- rep(1:2, 2)[jjl2]
                jlwd2 <- rep(1:2, each=2)[jjl2]
            } else {
                jlty2 <- 1:2
                jlwd2 <- c(2,2)
            }
            jlwd2 <- 2*jlwd2
            
            for (j in jjl2) {
                for (l in i3i[[jjp2[[j]]]]) {
                    if(!is.na(l)) {
                        if (showPoints) 
                            points(attr(wambl[[jjp2[[j]]]], 'Date'),
                                   d$amob[[jjp2[[j]]]][, l], 
                                   pch=j, col=scol[l])
                        lines(attr(wambl[[jjp2[[j]]]], 'Date'),
                              d$samob[[jjp2[[j]]]][, l],
                              lty=jlty2[j], lwd=jlwd2[j],
                              col=scol[l])
                    }
                }
            }
            abline(h=100)
            
        }

        if (any(plots%in%c(1:10))) {
            if (showPoints) {
                legend(legpos, allpls[-(1:4)][jjp2+6],
                       pch=jjp2, lty=jlty2, lwd=jlwd2,
                       bty='n')
            } else {
                legend(legpos, allpls[-(1:4)][jjp2+6], 
                       lty=jlty2, lwd=jlwd2,
                       bty='n')
            }
        } else {
            if (showPoints) {
                legend(legpos,
                       c(lll[oloc], allpls[-(1:4)][jjp2+6]),
                       pch=c(rep(1, length(oplot)), jjp2), 
                       lty=c(rep(1, length(oloc)),
                             rep(NA, length(jltyg2))),
                       lwd=c(rep(2, length(oloc)), jlwd2),
                       col=c(scol[oloc], rep(1, length(jjp2))), 
                       ncol=leg.ncols, bty='n')
            } else {
                legend(legpos,
                       c(lll[oloc], allpls[-(1:4)][jjp2+6]),
                       lty=c(rep(1, length(oloc)),
                             rep(1, length(jlty2))),
                       lwd=c(rep(2, length(oloc)), jlwd2),
                       col=c(scol[oloc], rep(1, length(jjp2))), 
                       ncol=leg.ncols, bty='n')
            }
        }
        
        axis(1, xl$x, format(xl$x, '%b,%d'))
        axis(2, las=1)
        segments(xl$x, rep(ylm3[1], length(xl$x)), 
                 xl$x, rep(ylm3[2], length(xl$x)), 
                 lty=2, col=gray(0.5, 0.5))    
        yy00 <- pretty(ylm3, 10) 
        segments(rep(xlm[1], length(yy00)), yy00,
                 rep(xlm[2], length(yy00)), yy00,
                 lty=2, col=gray(0.5, 0.5))    
        abline(h=0)
        
    }
    
    return(invisible())
}
