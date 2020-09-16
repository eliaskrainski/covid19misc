mv ~/Downloads/HIST_PAINEL_COVIDBR*.xlsx data/HIST_PAINEL_COVIDBR.xlsx 
time xlsx2csv data/HIST_PAINEL_COVIDBR.xlsx data/HIST_PAINEL_COVIDBR.csv
time R CMD BATCH --vanilla rcode/wdata-update.R 
cp data/wdl.RData covid19time/data/
scp -P2200 data/wdl.RData  data/HIST_PAINEL_COVIDBR.csv  elias@200.17.213.49:ShinyApps/covid19time/data/


