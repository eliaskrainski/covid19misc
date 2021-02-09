if (FALSE) { ## can manually skip

    setwd('..')

}

library(parallel)
(ncores <- as.integer(detectCores()/2))

Date <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
alldates <- gsub('-', '', as.character(Date))

if (!any(ls() %in% c('ussabb', 'uf')))
    source('rcode/ocommon.R')

if (!any(ls()=='gmob'))
    gmob <- TRUE

if (gmob) {
### mobility data from Google

    library(readr)
    ##system.time(gmbl <- read.csv(
    ##"data/Global_Mobility_Report.csv"))
    
    system.time(gmbl <- as.data.frame(read_csv(
                    "data/Global_Mobility_Report.csv",
                    col_types='cccccccciiiiii')))
    
    dim(gmbl)
    head(gmbl,2)
    summary(as.Date(unique(gmbl$date)))
    
    colnames(gmbl) <- gsub(
        '_percent_change_from_baseline', '', colnames(gmbl))

    for(j in which(sapply(gmbl, is.factor)))
        gmbl[, j] <- as.character(gmbl[, j])

    ##unique(grep('municip', gmbl$sub_region_2, val=T))
    ##unique(grep('city', gmbl$sub_region_2, val=T))
    ##length(unique(grep('City', gmbl$sub_region_2, val=T)))

    ##table(gmbl$country_region[grep('City', gmbl$sub_region_2)])
    ##unique(gmbl$sub_region_2[intersect(
      ##              grep('City', gmbl$sub_region_2),
        ##            which(gmbl$country_region=='United States'))])
    ##unique(grep('County', gmbl$sub_region_2, val=T))
    
    system.time(gmbl$fdate <- factor(gsub(
                    '-', '', gmbl$date), alldates))
    summary(as.integer(table(gmbl$fdate)))
    
    ##table(gmbl$country_region_code)
    
    ##table(as.character(
      ##  gmbl$sub_region_1[grep('State', gmbl$sub_region_1)]))

    ##system.time(gmbl$sub_region_1 <- gsub('State of ', '', gmbl$sub_region_1))
    system.time(iaux <- which(substr(gmbl$sub_region_1,1,9)=='State of '))
    system.time(gmbl$sub_region_1[iaux] <- substring(gmbl$sub_region_1[iaux], 10))

    system.time(gmbl$sub_region_2 <- gsub(' County', '', gmbl$sub_region_2))
    
    ussabb <- read.csv('data/us-states-abbreviation.csv')
    head(ussabb,2)
    for (j in 1:ncol(ussabb))
        ussabb[,j] <- as.character(ussabb[,j])

    for (j in 1:nrow(ussabb)) {
        iij <- which(gmbl$sub_region_1==ussabb$State[j])
        cat(j, as.character(ussabb$State)[j], length(iij), '\n')
        gmbl$sub_region_1[iij] <- ussabb$Postal[j]
    }

    for (j in 1:nrow(uf)) {
        iij <- which(gmbl$sub_region_1==uf$State[j])
        cat(j, as.character(uf$State)[j], length(iij), '\n')
        gmbl$sub_region_1[iij] <- uf$UF[j]
    }

    if (!any(ls()=='wdl'))
        system.time(load('data/wdl.RData'))
    
    length(aa <- paste(
               wdl[[1]]$City, 
               wdl[[1]]$Province,
               wdl[[1]]$Country, sep='_'))
    tail(sort(table(wdl[[1]]$Country)))
    table(duplicated(aa))
    table(aa[duplicated(aa)])

    if (FALSE) {

        wdl[[1]][grep('Caxias', aa), c(1:4, -2:0+ncol(wdl[[1]]))]

        length(bb <- unique(paste(
                   gmbl$sub_region_2,
                   gmbl$sub_region_1,
                   gmbl$country_region_code, sep='_')))
        tail(sort(table(gmbl$country_region))) 

        table(aa %in% bb)
        table(bb %in% aa)

    }

    gmbl$local <- paste(
        gmbl$sub_region_2,
        gmbl$sub_region_1,
        gmbl$country_region_code, sep='_')

    sort(table(gmbl$local[icode <- substr(
                              gmbl$local, 1, 2)=='__']))
    table(icode)
    
    gmbl$local[icode] <- paste0('__', gmbl$country_region[icode])
    gmbl$local[gmbl$local=='__United States'] <- '__US'

    if (FALSE) {

        table(aa %in% gmbl$local)
        table(unique(aa) %in% gmbl$local)
        table(unique(gmbl$local) %in% aa)
        
        length(unique(gmbl$local))
        length(unique(paste(gmbl$local, gmbl$metro)))
        
        table(w0 <- table(gmbl$local, gmbl$fdate))
        table(apply(w0>2, 1, any))
        table(apply(w0>2, 2, any))
        w0[apply(w0>2, 1, any), c(1:2, -9:0+ncol(w0))]
        
        head(gmbl,2)
        
        gmbl[which(gmbl$local=='__EG'), ]
        
    }

##stopifnot(all(table(gmbl$local, gmbl$fdate)<2))

    table(im.inc <- gmbl$local %in% aa)

    system.time(
        wgmbl <- mclapply(
            gmbl[im.inc, 9:14], tapply,
            gmbl[im.inc, c('local', 'fdate')], 
            mean, mc.cores=min(6L, ncores)))

    str(wgmbl[1])

    grep('Curitiba', rownames(wgmbl[[1]]))
    grep('Curitiba', rownames(wgmbl[[1]]), val=T)

    for (k in 1:length(wgmbl)) {
        ii <- grep('Curitiba_PR', rownames(wgmbl[[k]]))
        new <- wgmbl[[k]][ii,, drop=FALSE]
        rownames(new) <- gsub(
            'Curitiba', 'Curitiba(SM)', rownames(new))
        wgmbl[[k]] <- rbind(wgmbl[[k]], new)
    }

    for (k in 1:length(wgmbl)) {
        ii <- which(rownames(wgmbl[[k]])=='__Brazil')
        new <- wgmbl[[k]][ii,, drop=FALSE]
        rownames(new) <- gsub(
            '__Brazil', '__Brasil', rownames(new))
        wgmbl[[k]] <- rbind(wgmbl[[k]], new)
    }

    system.time(save(
        'wgmbl',
        file='data/wgmbl.RData',
        compress='xz'))

}

