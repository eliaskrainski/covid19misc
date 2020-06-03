if (FALSE)
    setwd('..')

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
for (fl in vnames)
    download.file(paste0(url0.csse, fl, '_global.csv'),
                  paste0('data/', fl, '_global.csv'))

### the official data from the Brazilian Health Ministry
###  is at https://covid.saude.gov.br/ 

### however, time series by municipalities
### area available at brazil.io
url.br <- paste0('https://data.brasil.io/',
                 'dataset/covid19/caso.csv.gz')

try(download.file(url.br, 'data/caso.csv.gv'), TRUE)

### Brazilian data put together by Wesley Cota
urlwc <- 'https://raw.githubusercontent.com/wcota/covid19br/master/'
wc.fl <- 'cases-brazil-cities-time.csv'
download.file(paste0(urlwc, wc.fl),
              paste0('data/', wc.fl))

### USdata
us.fl <- 'data/states_daily_4pm_et.csv'
us.url <- paste0('https://raw.githubusercontent.com/',
                 'COVID19Tracking/covid-tracking-data/master/',
                 us.fl)
download.file(us.url, us.fl)
