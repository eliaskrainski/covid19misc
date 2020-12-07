if (FALSE)
    setwd('..')

options(timeout=60*5)

### download the cssegis data (the famous one) 
### The base url for the data source 
url0.csse <- paste0(
    'https://raw.githubusercontent.com/',
    'CSSEGISandData/COVID-19/master/',
    'csse_covid_19_data/csse_covid_19_time_series/',
    'time_series_covid19_')

### The root names of the three derired  data files
vnames <- c('confirmed', 'deaths', 'recovered')

### (may or) may not download the files again  
system.time(
    for (fl in vnames)
        download.file(paste0(url0.csse, fl, '_global.csv'),
                      paste0('data/', fl, '_global.csv')))

### US counties data
download.file(
    'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv',
    'data/us-counties.csv')

### the official data from the Brazilian Health Ministry
###  is at https://covid.saude.gov.br/ 

### time series by municipalities
### area available at brazil.io
if (!any(ls()=='abrio'))
    abrio <- FALSE

if (abrio) {
    
    url.br <- paste0('https://data.brasil.io/',
                     'dataset/covid19/caso.csv.gz')
    
    system.time(try(download.file(
        url.br, 'data/caso.csv.gv'), TRUE))

}

if (!any(ls()=='awcota'))
    awcota <- FALSE

if (wcota) { ### Brazilian data put together by Wesley Cota

    urlwc <- 'https://raw.githubusercontent.com/wcota/covid19br/master/'
    wc.fl <- 'cases-brazil-cities-time.csv'
    
    system.time(
        download.file(paste0(urlwc, wc.fl),
                      paste0('data/', wc.fl))
    )
    
}

### USdata
us.fl <- 'data/daily.csv'
us.url <- paste0('https://api.covidtracking.com/v1/states/',
                 'daily.csv')
system.time(download.file(us.url, us.fl))

### DEST-UFMG data
system.time(download.file(paste0(
    'https://github.com/dest-ufmg/',
    'covid19repo/blob/master/data/',
    'cities.rds?raw=true'),
    'data/cities.rds'))

if (!any(ls()=='usesesa'))
    usesesa <- TRUE

if (usesesa) { ### DADOS SESA PR

    ldate <- Sys.Date()
    ldate

    CAP <- 0

    options(show.error.messages = FALSE)

    repeat {
        
        yyyy <- format(ldate, '%Y')
        mm <- format(ldate, '%m')
        dd <- format(ldate, '%d')

        fldt <- paste0(yyyy, '-', mm, '/informe_epidemiologico_',
                       dd, '_', mm, '_g')
        if (CAP>0)
            fldt <- toupper(fldt)
        
        sesa.fl <- paste0('https://www.saude.pr.gov.br/sites/default/',
                          'arquivos_restritos/files/documento/',
                          fldt, 'eral.csv')
        sesa.fl
        
        tmp <- try(download.file(
                sesa.fl, 'data/sesa-pr-geral.csv'))
        if (class(tmp)=='try-error') {
            if (CAP>1) {
                ldate <- ldate-1
                CAP <- 0
            } else {
                CAP <- CAP + 1
            }
        } else {
            break
        }

    }
    options(show.error.messages = TRUE)
    
    ## https://www.saude.pr.gov.br/sites/default/arquivos_restritos/files/documento/2020-11/informe_epidemiologico_08_11_geral.csv
    ## https://www.saude.pr.gov.br/sites/default/arquivos_restritos/files/documento/2020-11/informe_epidemiologico_08_11_obitos_casos_municipio.csv
    
    if (FALSE) {
        
        ses <- read.csv2('data/sesa-pr-geral.csv')
        head(ses)
        
        table(factor(ses$OBITO, c('Não', 'NÃO', '', 'Sim', 'SIM'),
                     rep(c('n', 's'), c(3,2))), ses$OBITO)
        table(factor(ses$OBITO, c('Não', 'NÃO', '', 'Sim', 'SIM'),
             rep(c('n', 's'), c(3,2))), ses$STATUS)
        
    }

}
