
if (FALSE) { ## can manually skip

    setwd('..')

}

options(width=70)

usems <- FALSE#TRUE
usefnd <- FALSE
wcota <- TRUE#FALSE
usebrio <- FALSE

if (!any(ls()=='dupdate'))
    dupdate <- TRUE

if (dupdate)
    system.time(source('rcode/getdata.R'))

Date <- seq(as.Date('20200121', '%Y%m%d'), Sys.Date(), 1)
alldates <- gsub('-', '', as.character(Date))

wwfun <- function(fl) {
    m <- read.csv(fl)
    co <- as.character(m$Country)
    prov.bl <- m$Province!=''
    toSum <- setdiff(
        co[prov.bl],
        do.call('intersect', split(co, c('x', 'y')[prov.bl+1])))
    if (length(toSum)>0) {
        aux <- data.frame(code='', City='', Province.State='',
                          Country.Region=toSum, Lat=NA, Long=NA) 
        cs <- sapply(toSum, function(x)
            colSums(m[which(m$Country==x), 5:ncol(m)]))
        return(rbind(data.frame(aux, t(cs)), 
                     data.frame(code='', City='', m)))
    } else {
        return(m)
    }                                                 
}

wdl <- lapply(c(confirmed='data/confirmed_global.csv',
                deaths='data/deaths_global.csv'), wwfun)
sapply(wdl, dim)
wdl[[1]][1:3, 1:7]
wdl[[1]][101:110, c(1:7, -1:0+ncol(wdl[[1]]))]

##for (k in 1:2) {
##    i.us <- which(wdl[[k]]$Country=='US')
##    wdl[[k]]$Country[i.us] <- 'United States'
##}

for (k in 1:2) {
    wdl[[k]]$Country.Region <- gsub(
        'Burma', 'Myanmar',
        wdl[[k]]$Country.Region, fixed=TRUE)
    wdl[[k]]$Country.Region <- gsub(
        'Congo (Brazzaville)', 'Congo',
        wdl[[k]]$Country.Region, fixed=TRUE)
    wdl[[k]]$Country.Region <- gsub(
        'Congo (Kinshasa)', 'DR Congo',
        wdl[[k]]$Country.Region, fixed=TRUE)
    wdl[[k]]$Country.Region <- gsub(
        'West Bank and Gaza', 'Palestine',
        wdl[[k]]$Country.Region, fixed=TRUE)
}

for (k in 1:2) {
    dtmp <- as.Date(colnames(wdl[[k]])[7:ncol(wdl[[k]])],
                    'X%m.%d.%y')
    nnt1 <- as.integer(difftime(
        head(dtmp,1), Date[1]), units='days')
    if (nnt1>0)
        for (j in 1:nnt1)
            wdl[[k]] <- data.frame(wdl[[k]][, 1:6],
                                   old=NA,
                                   wdl[[k]][, 7:ncol(wdl[[k]])])
    nnt <- as.integer(difftime(
        Sys.Date(), tail(dtmp,1), units='days'))
    if (nnt>0)
        for (j in 1:nnt)
            wdl[[k]] <- data.frame(wdl[[k]][, 1:ncol(wdl[[k]])],
                                   new=NA)
}

head(colnames(wdl[[1]]), 10)
tail(colnames(wdl[[1]]))

colnames(wdl[[1]])[7:ncol(wdl[[1]])] <-
    colnames(wdl[[2]])[7:ncol(wdl[[2]])] <-
    paste0('X', gsub('-', '', as.character(Date)))

### Add countries population
wcpop <- read.table(
    'data/world2020population.txt', header=TRUE)
head(wcpop)

w2i.c <- pmatch(paste0(wdl[[1]]$Country.Region,
                      wdl[[1]]$Province.State),
               wcpop$Country)
summary(w2i.c)

if (FALSE)
    wdl[[1]][is.na(w2i.c), 1:4]

summary(wpop.c <- wcpop$Population[w2i.c])

