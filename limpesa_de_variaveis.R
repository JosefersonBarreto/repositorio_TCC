
read.csv("sivep_gripe_2020.csv")
rm(list = ls())
library("readxl")
 dados<- read.csv("sivep_gripe_2020.csv",sep = ";",fileEncoding = "Latin1", check.names = F)
 
 data <- read_excel("data.xlsx")
 
 data %>% filter( data$setting == 'BRAZIL')

 # tentamos usar o argumento fileEncoding = "UCS-2LE" para ler o nosso banco de dados,
 # embora tenha rodado normalmente o código estava demorando muito para ler o conjunto de dados 
 # que contém mais de 92 mil observações,então opitei por substituir o argumento por outro, utilizei no
 # lugar o argumento,**check.names = F**  e o banco de dados foi carregado em segundos,
 # isso trás uma reflexão, não é porque um codigo esteja rodadando sem retorna algum erro que ele
 # seja o código mais adequado para aquela tarefa.
 
 library(dplyr)
 dados %>% select(Outcome)  %>%  table()
 
 
 covid<-read.csv("sivep_gripe_2020.csv",sep = ";",header = T,fileEncoding = "Latin1", check.names = F)
 
 
 
 dados2<-read.csv("INFLUD22-24-10-2022.csv",sep = ";",header = T,fileEncoding = "Latin1", check.names = F)

 
 
 t<-colnames(dados2) 
 oi<-data.frame(t)
 
 
 
 
 # fileEncoding = "UCS-2LE" para ler o nosso banco de dados,
 # embora tenha rodado normalmente o código estava demorando muito para ler o conjunto de dados
 # que contém mais de 92 mil observações,então opitei por substituir o argumento por outro, utilizei no
 # lugar o argumento,**check.names = F**  e o banco de dados foi carregado em segundos,
 # isso trás uma reflexão, não é porque um codigo esteja rodadando sem retorna algum erro que ele
 # seja o código mais adequado para aquela tarefa.
 
 
 
unique(dados2$CLASSI_FIN)


t=dados2 %>% filter(  [1:166] != "NA" ) 
table(dados2$CLASSI_FIN)
 





# removendo os NA da nossa base 


library(dplyr)
dados_sem_na <- dados2 %>% filter(complete.cases(.))
dados2$DT_NOTIFIC

filter<-dados2 |> select(DT_NOTIFIC:CLASSI_FIN)




dados3<-dados2 |> select(PCR_VSR:PCR_OUTRO) %>%  
  mutate_all(~replace(., is.na(.), 'não marcado'))



filter[96:106] <- filter |> select(PCR_VSR:PCR_OUTRO) %>%  
  mutate_all(~replace(., is.na(.), 'não marcado'))



# novos dados com a variavel resposta filtrada 

novos_dados =filter |> filter(CLASSI_FIN != 'NA')

# a coluna SURTo SG esta completamente vazia ,vamos removela 

sum(is.na(novos_dados$SURTO_SG))


novos_dados <- select(novos_dados, -SURTO_SG)




## removendo outras variaveis que não são interessantes para o modelo 

CS_ETINIA

novos_dados <- select(novos_dados, -DT_NOTIFIC:-CS_ZONA)


novos_dados <- select(novos_dados, -VACINA:-DT_2_DOSE)


novos_dados <- select(novos_dados, -HOSPITAL:-DT_SAIDUTI)


sum(is.na(novos_dados$OUT_ANTIV))


sum(novos_dados$OUT_ANTIV != '')/sum(nrow(novos_dados))

# temos uma proporção muito baixa nessa variavel,vamos removela 


novos_dados <- select(novos_dados, -DT_PCR:-POS_PCROUT)

write.csv(novos_dados, file = "novos_dados_limpos.csv", row.names = FALSE)

