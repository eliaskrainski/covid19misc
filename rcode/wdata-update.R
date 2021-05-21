
if (FALSE) { ## can manually skip

    setwd('..')

}


options(width=70)

library(parallel)
(ncores <- as.integer(detectCores()/2))

source('rcode/getdata.R')
source('rcode/ocommon.R')

(fls <- paste0('rcode/', c('wdl', 'wgmbl', 'wambl', 'wvac'), '-update.R'))

dupdate <- FALSE
mclapply(fls, source, mc.cores=min(4,ncores))

