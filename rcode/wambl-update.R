if (FALSE) { ## can manually skip

    setwd('..')

}

library(parallel)
(ncores <- as.integer(detectCores()/2))

if (!any(ls() %in% c('ussabb', 'uf')))
    source('rcode/ocommon.R')

## Apple mobility data
if (!any(ls()=='amob'))
    amob <- TRUE

if (amob) {

    (afl <- system('ls data/applemobilitytrends*csv', TRUE))
    afl <- afl[order(as.Date(
        substr(afl, 26, 35)), decreasing=TRUE)]
    afl

    system.time(wambl0 <- read.csv(afl[1]))
    dim(wambl0)

    wambl0[1:5, 1:10]
    table(wambl0[,1])

    for (j in 1:6)
        wambl0[, j] <- as.character(wambl0[,j])

    grep('Curitiba', wambl0$region, val=TRUE)
    wambl0[grep('Curitiba', wambl0$region), 1:7]

    tail(sort(table(wambl0$region[grep('County', wambl0$region)])))
    tail(sort(table(wambl0$region[grep('City', wambl0$region)])))
    tail(sort(table(wambl0$region[grep('Municipio', wambl0$region)])))

    wambl0$region <- gsub(' County', '', wambl0$region)
    wambl0$region <- gsub(' City', '', wambl0$region)
    wambl0$region <- gsub(' Municipio', '', wambl0$region)

    table(wambl0$region=='Brazil')
    wambl0[grep('Brazil', wambl0$region), 1:7]
    wambl0[grep('Brazil', wambl0$country), 1:7]
    table(wambl0$region=='United States')
    table(wambl0$sub.region=='Brazil')
    table(wambl0$sub.region=='United States')

    table(wambl0$region=='US')
    wambl0$region <- gsub(
        'United States', 'US', wambl0$region)
    table(wambl0$country=='US')
    wambl0$country <- gsub(
        'United States', 'US', wambl0$country)

    table(wambl0$region=='Brazil')
###    wambl0$region <- gsub('Brazil', 'BR', wambl0$region)
    table(wambl0$country=='Brazil')
###    wambl0$country <- gsub('Brazil', 'BR', wambl0$country)
    
    grep('Distrito', wambl0$region, val=T)
    grep('Distrito', wambl0$sub.region, val=T)
    grep('Distrito', wambl0$country, val=T)

    wambl0$region <- gsub(
        'Distrito Federal (Brazil)',
        'DF', wambl0$region, fixed=TRUE)

    grep('state', wambl0$region, val=T)
    grep('state', wambl0$sub.region, val=T)
    grep('state', wambl0$country, val=T)

    wambl0$region <- gsub(
        ' (state)', '', wambl0$region, fixed=TRUE)
    wambl0$sub.region <- gsub(
        ' (state)', '', wambl0$sub.region, fixed=TRUE)

    head(ussabb,2)
    for (j in 1:nrow(ussabb)) {
        ii <- which(wambl0$region==ussabb$State[j])
        cat(j, ussabb$State[j], length(ii), '')
        wambl0$region[ii] <- ussabb$Postal[j]
        ii <- which(wambl0$sub.region==ussabb$State[j])
        cat(length(ii), '')
        wambl0$sub.region[ii] <- ussabb$Postal[j]
        ii <- which(wambl0$country==ussabb$State[j])
        cat(length(ii), '\n')
        wambl0$country[ii] <- ussabb$Postal[j]
    }
    
    head(uf,2)
    for (j in 1:nrow(uf)) {
        ii <- which(wambl0$region==uf$State[j])
        cat(j, uf$State[j], length(ii), '')
        wambl0$region[ii] <- uf$UF[j]
        ii <- which(wambl0$sub.region==uf$State[j])
        cat(length(ii), '')
        wambl0$sub.region[ii] <- uf$UF[j]
        ii <- which(wambl0$country==uf$State[j])
        cat(length(ii), '\n')
        wambl0$country[ii] <- uf$UF[j]
    }
    
    grep('Bras', wambl0$region)
    grep('Bras', wambl0$alternative_name, val=T)
    grep('Bras', wambl0$sub.region)
    grep('Bras', wambl0$country)

    table(wambl0$transportation)

    wambl <- split(wambl0,
                   wambl0$transportation_type)
    sapply(wambl, dim)

    for(k in 1:length(wambl)) {
        wambl[[k]]$geo_type <- as.character(wambl[[k]]$geo_type)
        wambl[[k]]$region <- as.character(wambl[[k]]$region)
        wambl[[k]]$sub.region <- as.character(wambl[[k]]$sub.region)
        wambl[[k]]$country <- as.character(wambl[[k]]$country)
    }

    wambl0[grep('Curitiba', wambl0$region), 1:7]
    wambl0[grep('Para', wambl0$region), 1:6]
    wambl0[grep('Brazil', wambl0$region), 1:6]

    wambl0[grep('Paulo', wambl0$region), 1:6]

    wambl0[which(wambl0$region=='BR'), 1:7]

    table(wambl0$geo_type)
    
    for (k in 1:length(wambl)) {
        
        tlocal <- paste(
            ifelse(wambl[[k]]$geo_type %in% c('city', 'county'),
                   wambl[[k]]$region, ''), 
            ifelse(wambl[[k]]$geo_type %in% c('sub-region'), 
                   wambl[[k]]$region, wambl[[k]]$sub.region), 
            ifelse(wambl[[k]]$geo_type %in% c('country/region'), 
                   wambl[[k]]$region, wambl[[k]]$country), sep='_')

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
        file='data/wambl.RData',
        compress='xz'))

}
