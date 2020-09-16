if (FALSE)
    setwd("..")

if (FALSE) {

    flz <- 'gpr_000b11a_e.zip'
    if (FALSE) 
        download.file(
            paste0('http://www12.statcan.gc.ca/',
                   'census-recensement/2011/',
                   'geo/bound-limit/files-fichiers/', fl),
            paste0('maps/', flz))
    unzip(paste0('maps/', flz), exdir='maps/')
    dir('maps')
}

if (file.exists('maps/canada-sp.RData')) {

    load('maps/canada-sp.RData')

} else {

    library(maps)
    ca <- map("world", "Canada", fill=TRUE,
              col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(ca$names, ":"), function(x) x[1])
    ca.sp <- map2SpatialPolygons(
        ca, IDs=IDs,
        proj4string=CRS("+proj=longlat +datum=WGS84"))

    save('ca.sp', file='maps/canada-sp.RData', compress='xz')

}


if (file.exists('maps/caprovs.RData')) {
    load('maps/caprovs.RData')
} else {
    
    library(rgdal)
    ca.prov.full <- readOGR(
        'maps/gpr_000b11a_e.shp',
        encoding='latin1') 

    if (FALSE) { ### does not make big saving
        library(rgeos)
        
        ca.prov <- SpatialPolygonsDataFrame(
            gSimplify(ca.prov.full,
                      tol=0.05, topologyPreserve=TRUE),
        ca.prov.full@data)
        object.size(ca.prov.full)/object.size(ca.prov)
    } else {
        ca.prov <- ca.prov.full
    }
    
    if (FALSE) {

        par(mar=c(0,0,0,0), xaxs='i', yaxs='i')
        plot(ca.prov)
        plot(ca.prov.full, add=TRUE, lty=3)

    }
    
    save('ca.prov',
         file='maps/caprovs.RData',
         compress='xz')

}

