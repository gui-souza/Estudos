library(readxl)
library(XLConnect)
#library(xts)
library(tseries)
library(ggplot2)
#library(foreceitaast)
library(foreign)
library(dplyr)
library(stringr)


MontaSerieReceitas <- function(filepath, comCedae = TRUE) {
    
    dadosReceitaRJ <- loadWorkbook(filepath, create = TRUE)
    
    sheetNames <- getSheets(dadosReceitaRJ)
    numSheets <- length(excel_sheets(filepath))
    
    
    out <- data.frame(matrix(ncol = 0, nrow = 0))
    
    #currentYear <- yearStart
    #currentYear <- 2009
    
    for (i in 1:numSheets) {
        #implementando verificação pelo nome da aba
        #se tem 'com ...'
        temCom <- grepl("com", sheetNames[i], ignore.case = T)
        temSem <- grepl("sem", sheetNames[i], ignore.case = T)
        
        #leio abas que tem "com cedae" ou "sem cedae" comparando com parâmetro @comCedae 
        #OU abas que NÃO tem nem "com cedae" e nem "sem cedae"
        if(((temCom == TRUE & comCedae == TRUE)  | (temSem == TRUE & comCedae == FALSE)) |
           (temCom == FALSE & temSem == FALSE)) {
            #quero ver abas "com cedae"
            receitaAnoRj <- readWorksheet(dadosReceitaRJ, sheet = i) %>% 
                #receitaAnoRj <- readWorksheet(dadosReceitaRJ, sheet = 30) %>% 
                filter(.[,1] == "Total Geral" ) %>% 
                slice(1) %>% 
                select(-c(1, 2, 15:ncol(.)))
            
            #populando saída
            df <- data.frame(matrix(nrow = 12, ncol = 2))
            
            #coletar o ano (XXXX) da label da aba
            anoDaAba <- substr(sheetNames[i], 1, 4)
            mesAno <- c()
            
            #for (j in 1:12) mesAno <- c(mesAno, paste(j,'/', currentYear, sep = ''))
            for (j in 1:12) mesAno <- c(mesAno, paste(j,'/', anoDaAba, sep = ''))
            
            #receita <- as.numeric(as.vector(receitaAnoRj[1,]))
            names(receitaAnoRj) <- NULL
            receita <- unlist(receitaAnoRj)
            
            df <- cbind(mesAno, receita)
            out <- rbind(out, df)
        }
    }
    out
}

df <- MontaSerieReceitas(filepath = "serie_historica_ate_2017.xls", comCedae = T)

#df <- df %>% 
#    mutate(receita_temp = receita) %>% 
#    mutate(dot_count = str_count(receita, '\\.')) %>% 
#    mutate(receita_temp = ifelse(dot_count == 3, 
#                                 gsub('\\.', '', as.factor(receita_temp)), 
#                                 gsub('\\,', '.',as.factor(receita_temp))
#                                 )) %>% 
#    mutate(receita_temp = ifelse(dot_count == 3,
#                                 gsub('\\,', '.',as.factor(receita_temp)),
#                                      receita_temp)) %>% 
#    select(-c(dot_count, receita)) %>% 
#    rename(., receita = receita_temp)

repl = function(x)setNames(c("","."),c(".",","))[x]
df[,2] <- str_replace_all(as.character(df[,2]), "[.](?!\\d+$)|,", repl)

df <- df %>% mutate(receita = as.numeric(receita))









