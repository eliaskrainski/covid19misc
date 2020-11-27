
if (FALSE)
    setwd('..')

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/CasosCovid19/'

k <- 0
repeat {
    ufl <- paste0(u0,  Sys.Date() + k,
                  '_Casos_Covid_19_-_Base_de_Dados.csv')
    if (class(try(download.file(ufl, 'data/casosCuritibaSM.csv'),
                  TRUE))=='try-error') {
        k <- k-1
    } else {
        break
    }
}

dcwb <- read.csv2('data/casosCuritibaSM.csv', encoding='latin1')
head(dcwb)

dcwb$date <- as.Date(dcwb[,2], '%d/%m/%Y')
summary(dcwb$date)

if (!any(ls()=='alldates'))
    alldates <- gsub('-', '', seq(as.Date('20200122', '%Y%m%d'),
                    Sys.Date(), 1))

dcwb$fdate <- factor(gsub('-', '', 
                          dcwb$date,
                          fixed=TRUE), alldates)
tail(dcwb)

t3 <- table(dcwb$EVOLU, dcwb$fdate)
str(t3)

t3a <- apply(t3, 1, cumsum)

t3a[-20:0+nrow(t3a), ]

(jj <- pmatch(paste0('202011', 21:27), 
              colnames(t3)))
t3a[jj, ]

t3a[jj, ] <- cbind(
    c(##5705, 6449, 6849, 7449, 7714, 8415, 9131,
        9647, 10224, 11232, 11500, 12139, 12784, 12973),
    c(##1559, 1564, 1569, 1582, 1593, 1602, 1613,
        1621, 1628, 1638, 1649, 1660, 1678, 1694),
    c(##52084, 52238, 52484, 52704, 53342, 54013, 54695,
        55561, 55951, 56272, 57094, 58041, 58982, 60348))

if (all((t3a[nrow(t3a),] - 
         t3a[nrow(t3a)-1,])<0))
    t3a[nrow(t3a), ] <- t3a[nrow(t3a)-1, ]

tail(t3a, 14)
tail(rowSums(t3a), 14)

wcwb <- list(casos=rowSums(t3a), 
             obitos=t3a[,2])
str(wcwb)

##plot(diff(c(0,wcwb[[1]])))

print(sapply(wcwb, function(x) tail(x, 14)))

