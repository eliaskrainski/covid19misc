#!/bin/sh

cd /home/elias/covid19misc/

git pull

R CMD BATCH --vanilla rcode/dados-vacinacao-brasil.R

R CMD BATCH --vanilla rcode/vacinacao_brasil.R

git pull

R CMD BATCH --vanilla rcode/wdl-update.R 

R CMD BATCH --vanilla rcode/wgmbl-update.R 

R CMD BATCH --vanilla rcode/wambl-update.R 

R CMD BATCH --vanilla rcode/wvac-update.R 

cp data/w*.RData covid19time/data/ 
cp data/boletinsSMCuritiba.csv covid19time/data/ 
cp rcode/dados-curitiba.R covid19time/rcode/ 

cp data/wdl.RData /home/elias/ShinyApps/covid19time/data/ 
cp data/wgmbl.RData /home/elias/ShinyApps/covid19time/data/ 
cp data/wambl.RData /home/elias/ShinyApps/covid19time/data/ 
cp data/wvac.RData /home/elias/ShinyApps/covid19time/data/ 

cp data/v2tab.RData /home/elias/ShinyApps/covid19vacina/RData/ 

rm -rf data/vacinacao/*.csv -rf

