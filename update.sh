#!/bin/sh

cd /home/eliask/github/covid19misc/

R CMD BATCH --vanilla rcode/dados-vacinacao-brasil-uf.R

R CMD BATCH --vanilla rcode/vacinacao_brasil.R

R CMD BATCH --vanilla rcode/wdl-update.R 

R CMD BATCH --vanilla rcode/wgmbl-update.R 

R CMD BATCH --vanilla rcode/wambl-update.R 

R CMD BATCH --vanilla rcode/wvac-update.R 

cp data/w*.RData covid19time/data/
cp data/boletinsSMCuritiba.csv covid19time/data/
cp rcode/dados-curitiba.R covid19time/rcode/


