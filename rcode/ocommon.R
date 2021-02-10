if (FALSE)
    setwd('..')

ussabb <- read.csv('data/us-states-abbreviation.csv')
head(ussabb,2)
for (j in 1:ncol(ussabb))
    ussabb[,j] <- as.character(ussabb[,j])

nycc <- c('New York', 'Kings', 'Bronx', 'Richmond', 'Queens')

### BR states
uf <- data.frame(
    STATE=c("SERGIPE", "MARANHÃO", "ESPÍRITO SANTO", "AMAZONAS",
            "RORAIMA", "GOIÁS", "AMAPÁ", "RIO GRANDE DO SUL",
            "PARAÍBA", "PIAUÍ", "SÃO PAULO", "SANTA CATARINA",
                "PERNAMBUCO", "RIO DE JANEIRO", "MATO GROSSO DO SUL",
            "MATO GROSSO", "BAHIA", "MINAS GERAIS", "ALAGOAS",
            "CEARÁ", "RIO GRANDE DO NORTE", "PARANÁ", "RONDÔNIA",
            "DISTRITO FEDERAL", "ACRE", "PARÁ", "TOCANTINS"),
    State=c('Sergipe', 'Maranhão', 'Espírito Santo', 'Amazonas',
            'Roraima', 'Goiás', 'Amapá', 'Rio Grande do Sul',
            'Paraíba', 'Piauí', 'São Paulo', 'Santa Catarina',
            'Pernambuco', 'Rio de Janeiro', 'Mato Grosso do Sul',
            'Mato Grosso', 'Bahia', 'Minas Gerais', 'Alagoas',
            'Ceará', 'Rio Grande do Norte', 'Paraná', 'Rondônia',
            'Distrito Federal', 'Acre', 'Pará', 'Tocantins'), 
    UF = c("SE", "MA", "ES", "AM", "RR", "GO", "AP", "RS", "PB",
           "PI", "SP", "SC", "PE", "RJ", "MS", "MT", "BA", "MG",
           "AL", "CE", "RN", "PR", "RO", "DF", "AC", "PA", "TO"),
    row.names=c("28", "21", "32", "13", "14", "52", "16", "43", "25",
                "22", "35", "42", "26", "33", "50", "51", "29", "31",
                "27", "23", "24", "41", "11", "53", "12", "15", "17"))
