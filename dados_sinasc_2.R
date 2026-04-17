library(dplyr)
library(readr)

#Tarefa 1

dados_sinasc = read_csv2("SINASC_2015.csv")
dim(dados_sinasc)
str(dados_sinasc)
summary(dados_sinasc)

#Tarefa 2

dados_sinasc_1 = dados_sinasc[, c(1,4,5,6,7,12,13,14,15,19,21,22,23,24,35,38,44,46,48,59,60,61)]
colnames(dados_sinasc_1) = c("CONTADOR","CODMUNNASC","LOCNASC","IDADEMAE","ESTCIVMAE","CODMUNRES","GESTACAO","GRAVIDEZ","PARTO","SEXO","APGAR5","RACACOR","PESO","IDANOMAL","ESCMAE2010","RACACORMAE","SEMAGESTAC","CONSPRENAT","TPAPRESENT","TPROBSON","PARIDADE","KOTELCHUCK")

#Tarefa 3

dados_sinasc_1$CODMUNRES = as.character(dados_sinasc_1$CODMUNRES)
dados_sinasc_2 = dados_sinasc_1 %>% filter(substr(CODMUNRES, 1, 2) == "33")
nrow(dados_sinasc_2)
write_csv(dados_sinasc_2, "dados_sinasc_2.csv")

#Tarefa 4

vars_cat = c("LOCNASC","ESTCIVMAE","GESTACAO","GRAVIDEZ","PARTO","SEXO","RACACOR","IDANOMAL","ESCMAE2010","RACACORMAE","TPAPRESENT","TPROBSON","PARIDADE","KOTELCHUCK")
lapply(dados_sinasc_2[vars_cat], table)

#Tarefa 5

dados_sinasc_2 = dados_sinasc_2 %>% mutate(KOTELCHUCK = ifelse(KOTELCHUCK == 9, NA, KOTELCHUCK), 
                                           TPROBSON = ifelse(TPROBSON == 11, NA, TPROBSON), 
                                           APGAR5 = ifelse(APGAR5 == 99, NA, APGAR5), 
                                           PESO = ifelse(PESO == 9999, NA, PESO), 
                                           IDADEMAE = ifelse(IDADEMAE == 99, NA, IDADEMAE), 
                                           SEMAGESTAC = ifelse(SEMAGESTAC == 99, NA, SEMAGESTAC))

#Tarefa 6

dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO,
                              levels = c(1,2),
                              labels = c("Masculino","Feminino"))

dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK,
                                    levels = c(1,2,3,4,5),
                                    labels = c("Não realizou pré-natal",
                                               "Inadequado",
                                               "Intermediário",
                                               "Adequado",
                                               "Mais que adequado"))

dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON,
                                  levels = 1:10,
                                  labels = paste("Grupo", 1:10))

#Tarefa 7

dados_sinasc_2 = dados_sinasc_2 %>% mutate(
    
  #Peso
    F_PESO = case_when(
      PESO < 2500 ~ "Baixo peso",
      PESO >= 2500 & PESO < 4000 ~ "Peso normal",
      PESO >= 4000 ~ "Macrossomia",
      TRUE ~ NA_character_
    ),
    
    #Idade
    F_IDADE = cut(IDADEMAE,
                  breaks = c(-Inf,14,19,24,29,34,39,44,49,Inf),
                  labels = c("<15","15-19","20-24","25-29","30-34",
                             "35-39","40-44","45-49","50+")),
    
    #Apgar
    F_APGAR5 = case_when(
      APGAR5 < 7 ~ "Baixo",
      APGAR5 >= 7 ~ "Normal",
      TRUE ~ NA_character_
    ),
    
    #Peregrinação
    PERIG = ifelse(CODMUNNASC == CODMUNRES, "Não", "Sim"),
    
    #Estado civil
    ESTCIV = case_when(
      ESTCIVMAE %in% c(1,3,4) ~ "Sem companheiro",
      ESTCIVMAE %in% c(2,5) ~ "Com companheiro",
      TRUE ~ NA_character_
    )
  )

#Transformar em fator
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO)
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE)
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5)
dados_sinasc_2$PERIG = factor(dados_sinasc_2$PERIG)
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV)
