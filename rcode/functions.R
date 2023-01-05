
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
### fix an accumulated series to
### prevent negative differences
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

tSmooth <- function(y, Date, off = rep(0, length(y))) {
    d <- maked(y)
    X0 <- model.matrix(~0+w, data.frame(w=weekdays(Date)))
    j0 <- which.max(colSums(X0))
    jjx <- setdiff(1:7, j0)
    X <- X0[d$i, jjx, drop=FALSE]
    n <- length(y)
    nb <- round(n/14)
    B <- bs3f(1:length(Date), seq(1, n, length=nb))
    if (is.null(colnames(X)))
        colnames(X) <- paste0('x', 1:ncol(X))
    if (is.null(colnames(B)))
        colnames(B) <- paste0('b', 1:ncol(B))
    d <- maked(y)
    ni <- length(d$i)
    if (ni>9) {
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
                ##ys=d$y*exp(mean(x.m)-x.m),
                ys=exp(b.m+mean(x.m)),
                s=exp(drop(off[d$i] + mean(x.m) + b.m))))
}

### old vacinacao download function
### automatic download function
brvac.uf <- function(d=FALSE, uf=NULL) { 

    url0 <- paste0(
        'https://opendatasus.saude.gov.br/',
        'dataset/covid-19-vacinacao/resource/',
        '301983f2-aa50-4977-8fec-cfab0806cb0b')
    
### non elegant way to get the file names... 
    d0 <- readLines(url0)
    urls <- grep('c000.csv', d0, value=TRUE)[-1]
urls
    ufls <- sapply(urls, function(u) {
        g1 <- gregexpr('https', u)[[1]]
        g2 <- gregexpr('csv', u)[[1]]
        substr(u, g1, g2+2)
        }) ##substr(urls, 14, 155)
ufls
    if(!is.null(uf)) {
        ufls <- ufls[grep(uf, ufls)] 
    }
    lfls <- sapply(ufls, function(u) {
        gg <- gregexpr('/', u)[[1]]
        paste(substr(u, tail(gg,1)-2, tail(gg,1)-1),  
              substring(u, tail(gg,1)+1), sep='_')
    })
    names(lfls) <- sapply(ufls, function(u) {
        gg <- gregexpr('/', u)[[1]]
        substr(u, tail(gg,1)-2, tail(gg,1)-1)
    })
### download if asked and do not exists locally
    if(d) {
        for(j in 1:length(lfls)) {
            fl.j <- paste0('data/vacinacao/vac_', lfls[j])
            print(fl.j)
            if(!file.exists(fl.j))
                download.file(ufls[j], fl.j)
        }
    } 
    return(paste0('data/vacinacao/vac_', lfls))
}

