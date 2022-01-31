
if(FALSE)
    setwd('..')

t00 <- Sys.time()

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

library(data.table)

### desired variables
xsel <- c('paciente_idade', 'paciente_enumSexoBiologico',
          'paciente_endereco_coIbgeMunicipio', 'vacina_dataAplicacao',
          'vacina_descricao_dose', 'vacina_nome')
jj <- c(3, 5, 8, 28, 29, 31)

### automatic download function
brvac.parts <- function(d=FALSE, part=NULL) { 

    url0 <- paste0(
        'https://opendatasus.saude.gov.br/',
        'dataset/covid-19-vacinacao/resource/',
        '301983f2-aa50-4977-8fec-cfab0806cb0b')
    
### non elegant way to get the file names... 
    d0 <- readLines(url0)
    urls <- grep('c000.csv', d0, value=TRUE)
urls
    fls <- sapply(urls, function(u) {
        g1 <- gregexpr('https', u)[[1]]
        g2 <- gregexpr('csv', u)[[1]]
        substr(u, g1, g2+2)
        }) ##substr(urls, 14, 155)
    fls
   
    if(!is.null(part)) {
        pfls <- fls[grep(paste0('part-', sprintf('%05d', part)), fls)] 
    } else pfls <- fls
    pfls
    lfls <- sapply(pfls, function(u) {
        gg <- gregexpr('/', u)[[1]]
        paste(substr(u, tail(gg,2)[1]+1, tail(gg,1)-1),  
              substring(u, tail(gg,1)+1), sep='_')
    })
    lfls
    names(lfls) <- sapply(pfls, function(u) {
        gg <- gregexpr('/', u)[[1]]
        substr(u, tail(gg,1)+1, tail(gg,1)+10)
    })
    lfls
### download if asked and do not exists locally
    if(d) {
        for(j in 1:length(lfls)) {
            fl.j <- paste0('data/vacinacao/vac_', lfls[j])
            print(fl.j)
            if(!file.exists(fl.j))
                download.file(pfls[j], fl.j)
        }
    } 
    return(paste0('data/vacinacao/vac_', lfls))
}

print(fls <- brvac.parts(FALSE))

if(!any(ls()=='dupdate'))
    dupdate <- TRUE 
if(FALSE)
    dupdate <- FALSE

##if((!dupdate) & (length(system('ls RData'))<27))
  ## dupdate <- TRUE

###if(dupdate) {
    
options(timeout=60*300) ### to work with bad internet...

### download each part and retrieve the local file names: 'data/....csv"
fls <- brvac.parts(dupdate)

print(Sys.time()-t00)

cat("#####   P A R T 1   -   D O N E!   #####\n")

rdata <- TRUE 

if(file.exists('data/tMunDateN.csv')) {

  idat <- system('ls -lh data/tMunDateN.csv', TRUE)
  idat

  pdat <- gregexpr('data/', idat)[[1]]
  pdat

  ddat <- substr(idat, pdat-13, pdat-2)
  ddat

  dtime <- as.numeric(difftime(
      as.POSIXlt(Sys.time()),
     as.POSIXlt.character(ddat, format='%b %d %H:%M'),
      units='hours'))
  if(dtime>19) {
    rdata <- TRUE
  } else {
    rdata <- FALSE
  }

}

if(TRUE) {##(rdata) {
    
    parts <- substr(fls, 34, 38)
    parts
    
    mdd <- c('paciente_endereco_coIbgeMunicipio',
             'vacina_dataAplicacao', 
             'vacina_descricao_dose')
    
    verbose <- TRUE
    
    cat('cod6;Date;Dose;N\n',
        file='data/tMunDateN.csv')
    
    for (k in 1:length(fls)) {
        t1 <- Sys.time()
        
        if(verbose) cat(parts[k], ' read ... ')
        dvpart <- fread(fls[k], select=jj)
        
        if(verbose) cat(' RData ... ')
        save(list='dvpart',
             file=paste0('RData/dv_', parts[k], '.RData'))

        if(verbose) cat(' rm csv ... ')
	system(paste('rm', fls[k]))
        
        if(verbose) cat('tabulate ... ')
        tMunDate <- dvpart[,.N,by=mdd]
    
        if(verbose) cat('write ... ')
        write.table(tMunDate,
                    file='data/tMunDateN.csv',
                    append=TRUE,
                    sep=';',
                row.names=FALSE,
                col.names=FALSE,
                quote=FALSE)
        if(verbose) cat(Sys.time()-t1, ' ok\n')
        
    }
}

cat("#####   P A R T 2   -   D O N E!   #####\n")

print(Sys.time()-t00)
cat("#####  D O N E!   #####\n")

