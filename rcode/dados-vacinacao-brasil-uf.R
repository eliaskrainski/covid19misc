
if(FALSE)
    setwd('..')

t00 <- Sys.time()

### download from links available at 
### https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao/resource/ef3bd0b8-b605-474b-9ae5-c97390c197a8

### colnames of the files
fl1 <- system('ls data/vacinacao/*', TRUE)[1]
fl1

library(data.table)
tmp <- fread(fl1, nrows=0)
colnames(tmp)

### desired variables 
xsel <- colnames(tmp)[c(3, 5, 8, 28, 29, 31)] 
jj <- pmatch(xsel, colnames(tmp))
jj

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

if(dupdate) {
    
    options(timeout=60*300) ### to work with bad internet...
### download each UF local file and retrieve the local file names: 'data/UF_....csv"
    fls <- brvac.uf(TRUE)

    ufs <- substr(fls, 20, 21)
    ufs
    
    t1a <- Sys.time()
    for (k in 1:length(fls)) {
        t1 <- Sys.time()
        robj <- paste0('dv_', ufs[k])
        assign(robj,
               fread(fls[k], select=jj))
        save(list=robj,
             file=paste0('RData/', robj, '.RData'))
        print(Sys.time()-t1)
    }
    print(Sys.time()-t1a)

} else {

##    system.time(load('data/dvbr.RData'))
    ##  xsel <- colnames(dvbr)

    robjs <- dir('RData/')
    robjs

    library(data.table)

    mdd <- c('paciente_endereco_coibgemunicipio',
             'vacina_dataaplicacao', 
             'vacina_descricao_dose')

    cat('cod6;Date;Dose;N\n',
        file='data/tMunDateN.csv')

    rr <- lapply(robjs, function(rfl, verbose=TRUE) {
        cat('loading', rfl, '... ')
        attach(paste0('RData/', rfl)) ## safer and will warn about masked objects w/ same name in .GlobalEnv
        ufdv <- get(substr(rfl, 1, 5))
        detach()
        if(verbose) cat('loaded ', dim(ufdv), '')  
        tMunDate <- ufdv[,.N,by=mdd]
        if(verbose) cat('write ... ')
        write.table(tMunDate,
                    file='data/tMunDateN.csv',
                    append=TRUE,
                    sep=';',
                    row.names=FALSE,
                    col.names=FALSE,
                    quote=FALSE)
        if(verbose) cat('ok\n')
        return(NULL)
    })
    
}

print(Sys.time() - t00)
    
