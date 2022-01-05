if (FALSE) { ## can manually skip

    setwd('..')

}

t0 <- Sys.time()

##library(parallel)
##(ncores <- as.integer(detectCores()/2))

if (!any(ls() %in% c('ussabb', 'uf')))
    source('rcode/ocommon.R')

## Apple mobility data
if (!any(ls()=='amob'))
    amob <- TRUE

##if (amob) {

    (afl <- system('ls data/applemobilitytrends*csv', TRUE))
    afl <- afl[order(as.Date(
        substr(afl, 26, 35)), decreasing=TRUE)]
    afl

    system.time(wambl0 <- read.csv(afl[1]))
    dim(wambl0)

    wambl0[1:3, 1:11]

table(wambl0[,1])

grep('Paran', wambl0$region, value=TRUE)
wambl0[grep('Paran', wambl0$region), 1:7]
wambl0[grep('Paran', wambl0$sub.region), 1:7]

grep('Curitiba', wambl0$region, value=TRUE)

wambl0$region <- gsub('Sao Paulo', 'São Paulo', wambl0$region)

grep('São Paulo', wambl0$region, value=TRUE)
wambl0[grep('São Paulo', wambl0$region), 1:9]

grep('States', wambl0$region, val=T)

table(wambl0[,1])

    wambl0$region <- gsub(
        'Distrito Federal (Brazil)',
        'Distrito Federal', wambl0$region, fixed=TRUE)
    wambl0$region <- gsub(
        ' (state)', '', wambl0$region, fixed=TRUE)
    wambl0$sub.region <- gsub(
        ' (state)', '', wambl0$sub.region, fixed=TRUE)
    wambl0$region <- gsub(
        ' (Estado)', '', wambl0$region, fixed=TRUE)
    wambl0$sub.region <- gsub(
        ' (Estado)', '', wambl0$sub.region, fixed=TRUE)
    wambl0$region <- gsub(
        ' Province', '', wambl0$region, fixed=TRUE)
    wambl0$region <- gsub(
        ' Region', '', wambl0$region, fixed=TRUE)
    wambl0$region <- gsub(
        ' County', '', wambl0$region, fixed=TRUE)
    wambl0$sub.region <- gsub(
        ' County', '', wambl0$sub.region, fixed=TRUE)

    for (j in 1:6)
        wambl0[, j] <- as.character(wambl0[,j])

table(wambl0$geo_type)

table(idcc <- wambl0$geo_type%in%c('city', 'county'))
    for (j in 1:nrow(ussabb)) {
        ii <- which(idcc & (wambl0$sub.region==ussabb$State[j]))
        cat(j, ussabb$State[j], length(ii), '\n')
        wambl0$sub.region[ii] <- ussabb$Postal[j]
##        ii <- which(sub2b & (wambl0$sub.region==ussabb$State[j]))
  ##      cat(length(ii), '')
    ##    wambl0$sub.region[ii] <- ussabb$Postal[j]
    ##    ii <- which(sub2b & (wambl0$country==ussabb$State[j]))
      ##  cat(length(ii), '\n')
        ##wambl0$country[ii] <- ussabb$Postal[j]
    }
    
    head(uf,2)
    for (j in 1:nrow(uf)) {
        ii <- which(idcc & (wambl0$sub.region==uf$State[j]))
        cat(j, uf$State[j], length(ii), '\n')
        wambl0$sub.region[ii] <- uf$UF[j]
##        ii <- which(sub2b & (wambl0$sub.region==uf$State[j]))
  ##      cat(length(ii), '')
    ##    wambl0$sub.region[ii] <- uf$UF[j]
##        ii <- which(sub2b & (wambl0$country==uf$State[j]))
  ##      cat(length(ii), '\n')
    ##    wambl0$country[ii] <- uf$UF[j]
    }

wambl0[grep('PR', wambl0$sub.region), 1:7]

    wambl <- split(wambl0,
                   wambl0$transportation_type)
    sapply(wambl, dim)

    for(k in 1:length(wambl)) {
        wambl[[k]]$geo_type <- as.character(wambl[[k]]$geo_type)
        wambl[[k]]$region <- as.character(wambl[[k]]$region)
        wambl[[k]]$sub.region <- as.character(wambl[[k]]$sub.region)
        wambl[[k]]$country <- as.character(wambl[[k]]$country)
    }
    
    for (k in 1:length(wambl)) {

        tlocal <- 
            ifelse(wambl[[k]]$geo_type %in% c('city', 'county'),
                   paste0(wambl[[k]]$region, '_', 
                          wambl[[k]]$sub.region, '_', 
                          gsub('United States', 'US',
                               gsub('Brazil', 'BR', wambl[[k]]$country))), 
            ifelse(wambl[[k]]$geo_type %in% c('sub-region'), 
                   paste0('_', wambl[[k]]$region, '_',
                          gsub('United States', 'US',
                               gsub('Brazil', 'BR', wambl[[k]]$country))),
                   paste0('__', wambl[[k]]$region)))

        if(FALSE) {
            
            grep('tate', tlocal, val=TRUE)
            head(grep('US', tlocal, val=TRUE), 20)

        }
        
        tlocal <- gsub('__United States', '__US', tlocal)

        tmp <- as.matrix(wambl[[k]][, 7:ncol(wambl[[k]])])
        
        ii <- grep('Curitiba_PR', tlocal)
        if (length(ii)>0) {
            cat(ii, tlocal[ii], '\n')
            wacwb <- as.matrix(
                wambl[[k]][ii, 7:ncol(wambl[[k]]), drop=FALSE])
            ncwb <- gsub('Curitiba', 'Curitiba(SM)', tlocal[ii])
            rownames(wacwb) <- ncwb
            tmp <- rbind(tmp, wacwb)
            tlocal <- c(tlocal, ncwb)
            ncwb <- gsub('Curitiba', 'Curitiba(SMB)', tlocal[ii])
            rownames(wacwb) <- ncwb 
            tmp <- rbind(tmp, wacwb)
            tlocal <- c(tlocal, ncwb)
        }
        
        ii <- which(tlocal %in% paste0(c('NY', nycc[-1]), '_NY_US'))
        if (length(ii)>0) {
            print(ii)
            wanyc <- matrix(
                colMeans(wambl[[k]][ii, 7:ncol(wambl[[k]]),
                                    drop=FALSE]), 1)
            nnyc <- 'New York City_NY_US'
            rownames(wanyc) <- nnyc
            tmp <- rbind(tmp, wanyc)
            tlocal <- c(tlocal, nnyc)
        }
        
        wambl[[k]] <- tmp 
        attr(wambl[[k]], 'local') <- tlocal 
    }

if(FALSE) {

    load('data/wdl.RData')
    
    iii3 <- lapply(wambl, function(w)
        pmatch(attr(w, 'local'),
               paste0(wdl[[1]]$City, '_', wdl[[1]]$Prov, '_', wdl[[1]]$Coun)))

    sapply(iii3, function(x) table(is.na(x)))

    attr(wambl[[1]], 'local')[which(is.na(iii3[[1]]))]

}

    sapply(wambl, nrow)
    sapply(wambl, function(x) length(attr(x, 'local')))

    for (k in 1:length(wambl)) {
        l3l <- attr(wambl[[k]], 'local')
        grep('Curitiba', l3l,value=TRUE)
        
        grep('Bra', l3l, val=TRUE)
        l3l <- gsub('Brazil', 'Brasil', l3l)
        
        im.br <- intersect(which(substr(l3l, 1, 1)!='_'),
                           grep('Brasil', l3l))
        l3l[im.br] <- gsub('Brasil', 'BR', l3l[im.br])
        l3l[im.br]
        
        ibr <- which(l3l=='__Brasil')
        l3l[ibr]
        
        wambl[[k]] <- rbind(wambl[[k]], wambl[[k]][ibr,])
        attr(wambl[[k]], 'local') <- c(l3l, '__Brazil')
        attr(wambl[[k]], 'Date') <-
            as.Date(colnames(wambl[[k]]), 'X%Y.%m.%d')
    }
    
    
    system.time(save(
        'wambl',
        file='data/wambl.RData'))
##        compress='xz'))

##}

Sys.time()-t0
