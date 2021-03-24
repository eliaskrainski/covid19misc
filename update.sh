##mv ~/Downloads/HIST_PAINEL_COVIDBR*.xlsx data/HIST_PAINEL_COVIDBR.xlsx 
##time xlsx2csv data/HIST_PAINEL_COVIDBR.xlsx data/HIST_PAINEL_COVIDBR.csv
##unzip ~/Downloads/HIST_PAINEL_COVIDBR*.zip 
##mv HIST_PAINEL_COVIDBR*.csv data/HIST_PAINEL_COVIDBR.csv
time R CMD BATCH --vanilla rcode/wdata-update.R 
cp data/w*.RData covid19time/data/
cp data/dadosSMCuritiba.csv covid19time/data/
cp rcode/dados-curitiba.R covid19time/rcode/
scp -P2200 rcode/dados-curitiba.R elias@200.17.213.49:ShinyApps/covid19time/rcode/
scp -P2200 data/w*.RData data/dadosSMCuritiba.csv elias@200.17.213.49:ShinyApps/covid19time/data/

