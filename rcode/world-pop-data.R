
if (FALSE)
    setwd('..')

url <- paste0(
    'https://www.worldometers.info/',
    'world-population/population-by-country/')

library(XML)

dl <- readHTMLTable(readLines(url))
length(dl)

str(dl[[1]])

table(dl[[1]]$Country=='United States')

d1 <- data.frame(
    Country=gsub('United States', 'US',
                 as.character(dl[[1]]$Country)),
    sapply(dl[[1]][, -(1:2)], function(x)
        as.integer(gsub(
            ',', '',
            gsub('%', '', as.character(x),
                 fixed=TRUE)))))
str(d1)

colSums(is.na(d1))
d1[!complete.cases(d1), ]

dl[[1]][!complete.cases(d1), ]

load('data/wdl.RData')

table(wdl[[1]]$Country %in%
      c('BR', 'Brasil', d1$Country))
table(d1$Country %in% wdl[[1]]$Country)

ccno <- wdl[[1]]$Country[!(
    wdl[[1]]$Country %in%
    c('BR', 'Brasil', d1$Country))]
ccno

grep('Iv', d1$Country, val=TRUE)
grep('zec', d1$Country, val=TRUE)
grep('rinc', d1$Country, val=TRUE) ## no
grep('ore', d1$Country, val=TRUE)
grep('ov', d1$Country, val=TRUE)
grep('dam', d1$Country, val=TRUE) ## no
grep('Kitts', d1$Country, val=TRUE)
grep('ent', d1$Country, val=TRUE)
grep('ome', d1$Country, val=TRUE)
grep('wan', d1$Country, val=TRUE)
grep('alest', d1$Country, val=TRUE) 

ccr <- c(
    "Côte d'Ivoire", "Czech Republic (Czechia)", NA,
    "South Korea", NA, NA, "Saint Kitts & Nevis",
    "St. Vincent & Grenadines", "Sao Tome & Principe",
    "Taiwan", "State of Palestine")
for (j in which(!is.na(ccr)))
    d1$Country <- gsub(
        ccr[j], ccno[j], d1$Country, fixed=TRUE)

d1$Country[!(d1$Country %in% wdl[[1]]$Country)]

write.table(d1,
            file='data/world2020population.txt',
            row.names=FALSE)
