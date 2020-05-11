
if (FALSE)
    setwd('..') 

### official map source 
### http://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2018/Brasil/BR/

if (!any(ls()=='simplified'))
    simplified <- TRUE

if (file.exists('maps/bruf.RData') | 
    file.exists('maps/brufs.RData')) {
### load the map by municipalities
    if (simplified) {
        load('maps/brufs.RData')
    } else {
        load('maps/bruf.RData')
    }
} else {
    
### download the map by municipalities
    flz <- 'br_unidades_da_federacao.zip'
    if(!file.exists(paste0('maps/', flz))) {
        print(dir())
        url <- paste0('http://geoftp.ibge.gov.br/',
                      'organizacao_do_territorio/',
                      'malhas_territoriais/',
                      'malhas_municipais/',
                      'municipio_2018/Brasil/BR/')
        download.file(paste0(url, flz),
                      paste0('maps/', flz)) 
    }

### uncompress the file    
    unzip(paste0('maps/', flz), exdir='maps')

### read the map
    library(rgdal)
    bruf <- readOGR('maps/BRUFE250GC_SIR.shp')
    bruf@data

    bruf$UF <- c(
        'SE', 'MA', 'ES', 'AM', 'RR', 'GO', 'AP',
        'RS', 'PB', 'PI', 'SP', 'SC', 'PE', 'RJ',
        'MS', 'MT', 'BA', 'MG', 'AL', 'CE', 'RN',
        'PR', 'RO', 'DF', 'AC', 'PA', 'TO')
    
    bruf$pop2019 <-
        c(2298696, 7075181, 4018650, 4144597,
          605761, 7018354, 845731, 11377239,
          4018127, 3273227, 45919049, 7164788,
          9557071, 17264943, 2778986, 3484466,
          14873064, 21168791, 3337357, 9132078,
          3506853, 11433957, 1777225, 3015268, 
          881935, 8602865, 1572866)

    library(rgeos)
    brufs <- SpatialPolygonsDataFrame(
        gSimplify(bruf, 0.1, topologyPreserve=TRUE),
        bruf@data)

    object.size(bruf)/object.size(brufs) ## 5

### save it compressed in R object format
    save('brufs', file='maps/brufs.RData', compress='xz')

}

