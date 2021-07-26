
y2distribute <- function(x, M=1e9, jn=-20:0) {
    ii <- which(x>M)
    if(length(ii)>0) {
        d <- x[ii]-M
        nn <- length(jn)
        for(i in 1:length(ii)) {
            m <- ceiling(sum(x[jn+ii[i]])/nn)
            i2 <- which(x[jn+ii[i]]<=m)
            dm <- floor(d[i]/length(i2))
            for(j in i2) {
                x[(jn+ii[i])[j]] <- x[(jn+ii[i])[j]] + dm
                x[ii[i]] <- x[ii[i]]-dm
            }
        }            
    }
    return(x)
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

bs2f <- function(x, x0) {
    i <- findInterval(
        x, x0, all.inside=TRUE)
    x0 <- c(x0, x0[length(x0)]) 
    d <- diff(x0)
    d2 <- diff(x0, 2)
    ul <- (x-x0[i])/d[i]
    ur <- 1-ul
    xl <- ul^2*d[i+1]/d2[i+1]
    xr <- ur^2*d[i+1]/d2[i]
    if (require(Matrix))  {
        a <- sparseMatrix(
            i=rep(1:length(i), 3), 
            j=c(i+2L, i+1L, i),
            x=c(xl, 1-(xl+xr), xr))
    } else {
        a <- matrix(0, length(i), max(i)+2)
        a[rep(1:length(i), 3),
          c(i+2L, i+1L, i)] <- c(xl, 1-(xl+xr), xr)          
    }
    a[, 2] <- a[,1]+a[,2]
    a[, ncol(a)-1] <-
        a[, ncol(a)-1] + a[, ncol(a)]
    return(a[,2:(ncol(a)-1)])
}

bs3f <- function(x, x0, sparse=FALSE) {
    bb <- splines::bs(x, knots=x0, degree=3)
    bb <- bb[, which(colSums(bb)>0)]
    if(ncol(bb)>4) {
        bb[, 2] <- bb[, 1] + bb[, 2]
        bb[, ncol(bb)-1] <- bb[, ncol(bb)-1] + bb[, ncol(bb)]
        bb <- bb[, 2:(ncol(bb)-1)]
    }
    if(sparse)
        bb <- Matrix(as.matrix(bb))
    return(bb)
}

if(FALSE) {
    library(Matrix)
    system.time(bb <- bs2f(1:1e4, seq(1, 1e4, 10)))
    system.time(bbb <- bs3f(1:1e4, seq(1, 1e4, 10)))
    object.size(bb)
    object.size(bbb)
    object.size(bbb)/object.size(bb)
}

maked <- function(y) {
    i1 <- which(y>0)[1]
    ii <- i1:length(y)
    y.s <- y[ii]
    y.s[y.s<0] <- NA
    nna <- !is.na(y.s)
    list(i=ii[nna], y=y.s[nna]) 
}

tSmooth <- function(y, X, B, off = rep(0, length(y))) {
    if (is.null(colnames(X)))
        colnames(X) <- paste0('x', 1:ncol(X))
    if (is.null(colnames(B)))
        colnames(B) <- paste0('b', 1:ncol(B))
    d <- maked(y)
    ni <- length(d$i)
    if (ni>9) {
        X <- X[d$i, , drop=FALSE]
        sxx <- diag(crossprod(X))
        jx <- which(sxx>0)
        if (length(jx)>0) {
            X <- X[, jx, drop=FALSE]
            nxx <- ncol(X)
            jx <- 1:nxx
        }
    }
    if((ni<15) | (nxx==1)) { 
        X <- matrix(1, ni, 1)
        colnames(X) <- 'x0'
        jx <- 1
    }
    if ((ni>21) | (nxx>1)) {
        B <- B[d$i,, drop=FALSE]
        sbb <- diag(crossprod(B))
        jb <- which(sbb>0.0)
        B <- B[, jb, drop=FALSE]
        sbb <- sbb[jb]
        if (sbb[1]<mean(sbb)) {
            B[, 2] <- B[, 1] + B[, 2]
            B <- B[, 2:ncol(B), drop=FALSE]
            sbb[2] <- sbb[1] + sbb[2]
            sbb <- sbb[-1]
        }
        nbb <- ncol(B)
        if (sbb[nbb]<mean(sbb)) {
            B[, nbb-1] <- B[, nbb-1] + B[, nbb]
            B <- B[, 1:(nbb-1)]
            sbb[nbb-1] <- sbb[nbb-1] + sbb[nbb]
            sbb <- sbb[1:(nbb-1)]
        }
        nbb <- ncol(B)
        j <- 1:nbb
    } else {
        nbb <- 1
        B <- matrix(d$i-mean(d$i), ni, nbb)
        colnames(B) <- 'b1'
        jb <- 1
    }
    gfit <- glm.fit(cbind(X, B), d$y,
                    family=poisson(),
                    offset=off[d$i])
    bg <- gfit$coef

    bg <- bg[!is.na(bg)]
    icx <- pmatch(colnames(X), names(bg))
    iicx <- which(!is.na(icx))
    x.m <- drop(X[, iicx, drop=FALSE] %*% bg[icx[iicx]] )

    icb <- pmatch(colnames(B), names(bg))
    iicb <- which(!is.na(icb))

    b.m <- B[, iicb, drop=FALSE] %*% bg[icb[iicb]]    

    return(list(fit=gfit, i=d$i, y=d$y,
                ys=d$y*exp(mean(x.m)-x.m),
                s=exp(drop(off[d$i] + mean(x.m) + b.m))))
}

