
if(FALSE)
    setwd('..')

t00 <- Sys.time()

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

### colnames of the files
##fl1 <- system('ls data/vacinacao/*', TRUE)[1]
##fl1

library(data.table)

### desired variables
xsel <- c('paciente_idade', 'paciente_enumsexobiologico',
          'paciente_endereco_coibgemunicipio', 'vacina_dataaplicacao',
          'vacina_descricao_dose', 'vacina_nome')
jj <- c(3, 5, 8, 28, 29, 31)

### automatic download function
brvac.uf <- function(d=FALSE, uf=NULL) { 
    url0 <- paste0('https://opendatasus.saude.gov.br/',
                   'dataset/covid-19-vacinacao/resource/',
                   'ef3bd0b8-b605-474b-9ae5-c97390c197a8')
### non elegant way to get the file names... 
    d0 <- readLines(url0)
    urls <- grep('c000.csv', d0, value=TRUE)[-1]
    ufls <- substr(urls, 14, 155)
    if(!is.null(uf)) {
        ufls <- ufls[substr(ufls, 84, 85) %in% uf]
    }
    lfls <- gsub('/', '_', substring(ufls, 84), fixed=TRUE)
### download if asked and do not exists locally
    if(d) {
        for(j in 1:length(lfls)) {
            fl.j <- paste0('data/vacinacao/vac_', lfls[j])
            if(!file.exists(fl.j))
                download.file(ufls[j], fl.j)
        }
    } 
    return(paste0('data/vacinacao/vac_', lfls))
}

if(!any(ls()=='dupdate'))
    dupdate <- TRUE 
if(FALSE)
    dupdate <- FALSE

##if((!dupdate) & (length(system('ls RData'))<27))
  ## dupdate <- TRUE

###if(dupdate) {
    
options(timeout=60*300) ### to work with bad internet...
### download each UF local file and retrieve the local file names: 'data/UF_....csv"
fls <- brvac.uf(dupdate)

print(Sys.time()-t00)

cat("#####   P A R T 1   -   D O N E!   #####\n")

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
    
    ufs <- substr(fls, 20, 21)
    ufs
    
    mdd <- c('paciente_endereco_coibgemunicipio',
             'vacina_dataaplicacao', 
             'vacina_descricao_dose')
    
    verbose <- TRUE
    
    cat('cod6;Date;Dose;N\n',
        file='data/tMunDateN.csv')
    
    for (k in 1:length(fls)) {
        t1 <- Sys.time()
        
        if(verbose) cat(ufs[k], ' read ... ')
        ufdv <- fread(fls[k], select=jj)
        
        if(verbose) cat(' RData ... ')
        save(list='ufdv',
             file=paste0('RData/dv_', ufs[k], '.RData'))

	if(verbose) cat(' rm csv ...')
	system(paste('rm', fls[k]))
        
        if(verbose) cat('tabulate ... ')
        tMunDate <- ufdv[,.N,by=mdd]
    
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

