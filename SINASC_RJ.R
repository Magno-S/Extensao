library(dplyr)
library(readr)


#Tarefa 1

dados_sinasc = read_csv2("SINASC_2015.csv")
dim(dados_sinasc)
str(dados_sinasc)
summary(dados_sinasc)


#Tarefa 2

dados_sinasc_1 = dados_sinasc[, c(1,4,5,6,7,12,13,14,15,19,21,22,23,24,35,38,44,46,48,59,60,61)]


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
                                           SEMAGESTAC = ifelse(SEMAGESTAC == 99, NA, SEMAGESTAC),
                                           ESTCIVMAE = ifelse(ESTCIVMAE == 9, NA, ESTCIVMAE),
                                           GESTACAO = ifelse(GESTACAO == 9, NA, GESTACAO),
                                           GRAVIDEZ = ifelse(GRAVIDEZ == 9, NA, GRAVIDEZ),
                                           PARTO = ifelse(PARTO == 9, NA, PARTO),
                                           SEXO = ifelse(SEXO == 0, NA, SEXO),
                                           IDANOMAL = ifelse(IDANOMAL == 9, NA, IDANOMAL),
                                           ESCMAE2010 = ifelse(ESCMAE2010 == 9, NA, ESCMAE2010),
                                           TPAPRESENT = ifelse(TPAPRESENT == 9, NA, TPAPRESENT))


#Tarefa 6

dados_sinasc_2$LOCNASC = factor(
  dados_sinasc_2$LOCNASC,
  levels = c(1,2,3,4,5),
  labels = c(
    "Hospital",
    "Outros estabelecimentos de saúde",
    "Domicílio",
    "Outros",
    "Aldeia indígena"
  )
)

dados_sinasc_2$ESTCIVMAE = factor(
  dados_sinasc_2$ESTCIVMAE,
  levels = c(1,2,3,4,5),
  labels = c(
    "Solteira",
    "Casada",
    "Viúva",
    "Separada judicialmente",
    "União consensual"
  )
)

dados_sinasc_2$GESTACAO = factor(
  dados_sinasc_2$GESTACAO,
  levels = c(1,2,3,4,5,6),
  labels = c(
    "<22 semanas",
    "22 a 27 semanas",
    "28 a 31 semanas",
    "32 a 36 semanas",
    "37 a 41 semanas",
    "42 semanas e mais"
  )
)

dados_sinasc_2$GRAVIDEZ = factor(
  dados_sinasc_2$GRAVIDEZ,
  levels = c(1,2,3),
  labels = c(
    "Única",
    "Dupla",
    "Tripla e mais"
  )
)

dados_sinasc_2$PARTO = factor(
  dados_sinasc_2$PARTO,
  levels = c(1,2),
  labels = c(
    "Vaginal",
    "Cesáreo"
  )
)

dados_sinasc_2$SEXO = factor(
  dados_sinasc_2$SEXO,
  levels = c(1,2),
  labels = c(
    "Masculino",
    "Feminino"
  )
)

dados_sinasc_2$RACACOR = factor(
  dados_sinasc_2$RACACOR,
  levels = c(1,2,3,4,5),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indígena"
  )
)

dados_sinasc_2$IDANOMAL = factor(
  dados_sinasc_2$IDANOMAL,
  levels = c(1,2),
  labels = c(
    "Sim",
    "Não"
  )
)

dados_sinasc_2$ESCMAE2010 = factor(
  dados_sinasc_2$ESCMAE2010,
  levels = c(0,1,2,3,4,5),
  labels = c(
    "Sem escolaridade",
    "Fundamental I",
    "Fundamental II",
    "Médio",
    "Superior incompleto",
    "Superior completo"
  )
)

dados_sinasc_2$RACACORMAE = factor(
  dados_sinasc_2$RACACORMAE,
  levels = c(1,2,3,4,5),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indígena"
  )
)

dados_sinasc_2$TPAPRESENT = factor(
  dados_sinasc_2$TPAPRESENT,
  levels = c(1,2,3),
  labels = c(
    "Cefálica",
    "Pélvica ou podálica",
    "Transversa"
  )
)

dados_sinasc_2$TPROBSON = factor(
  dados_sinasc_2$TPROBSON,
  levels = 1:10,
  labels = paste("Grupo", 1:10)
)

dados_sinasc_2$PARIDADE = factor(
  dados_sinasc_2$PARIDADE,
  levels = c(1,2),
  labels = c(
    "Primípara",
    "Não primípara"
  )
)

dados_sinasc_2$KOTELCHUCK = factor(
  dados_sinasc_2$KOTELCHUCK,
  levels = c(1,2,3,4,5),
  labels = c(
    "Não realizou pré-natal",
    "Inadequado",
    "Intermediário",
    "Adequado",
    "Mais que adequado"
  )
)


#Tarefa 7

dados_sinasc_2 = dados_sinasc_2 %>% mutate(
    
  #Peso
  PESO = as.numeric(PESO),
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
  APGAR5 = as.numeric(APGAR5),
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


#Tarefa 8:

tabela_pig = read_csv2("Tabela_PIG_Brasil.csv")
str(tabela_pig)

#Fazer merge com dados principais
dados_sinasc_2 = dados_sinasc_2 %>%
  left_join(tabela_pig,
            by = c("SEXO", "SEMAGESTAC"))

#Criar F_PIG (gravidez única)
dados_sinasc_2 = dados_sinasc_2 %>%
  mutate(
    F_PIG = case_when(
      GRAVIDEZ == 1 &
        !is.na(PESO) &
        !is.na(PESO_P10) &
        !is.na(PESO_P90) &
        PESO < PESO_P10 ~ "PIG",
      
      GRAVIDEZ == 1 &
        !is.na(PESO) &
        !is.na(PESO_P10) &
        !is.na(PESO_P90) &
        PESO >= PESO_P10 &
        PESO <= PESO_P90 ~ "AIG",
      
      GRAVIDEZ == 1 &
        !is.na(PESO) &
        !is.na(PESO_P10) &
        !is.na(PESO_P90) &
        PESO > PESO_P90 ~ "GIG",
      
      TRUE ~ NA_character_
    )
  )

#Transformar em fator
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG)

#Tarefa 9

resumo_sinasc = function(base, nivel, codigo) {
  
  data.frame(
    
    # Identificação
    ANO = 2015,
    NIVEL = nivel,
    CODMUNRES = codigo,
    
    # Nascimentos
    TN = nrow(base),
    
    TNRC = sum(complete.cases(base)),
    
    TNRCR = sum(complete.cases(
      base[, c("CONTADOR","CODMUNNASC","LOCNASC","IDADEMAE",
               "ESTCIVMAE","CODMUNRES","GESTACAO","GRAVIDEZ",
               "PARTO","SEXO","APGAR5","RACACOR","PESO",
               "IDANOMAL","ESCMAE2010","RACACORMAE",
               "SEMAGESTAC","CONSPRENAT","TPAPRESENT",
               "TPROBSON","PARIDADE","KOTELCHUCK")]
    )),
    
    # Idade materna
    TGI_15 = sum(base$F_IDADE == "<15", na.rm = TRUE),
    TGI_15_19 = sum(base$F_IDADE == "15-19", na.rm = TRUE),
    TGI_20_24 = sum(base$F_IDADE == "20-24", na.rm = TRUE),
    TGI_25_29 = sum(base$F_IDADE == "25-29", na.rm = TRUE),
    TGI_30_34 = sum(base$F_IDADE == "30-34", na.rm = TRUE),
    TGI_35_39 = sum(base$F_IDADE == "35-39", na.rm = TRUE),
    TGI_40_44 = sum(base$F_IDADE == "40-44", na.rm = TRUE),
    TGI_45_49 = sum(base$F_IDADE == "45-49", na.rm = TRUE),
    TGI_50 = sum(base$F_IDADE == "50+", na.rm = TRUE),
    
    TGIF = sum(base$IDADEMAE >= 15 &
                 base$IDADEMAE <= 49,
               na.rm = TRUE),
    
    IM_P25 = quantile(base$IDADEMAE, 0.25, na.rm = TRUE),
    IM_P50 = quantile(base$IDADEMAE, 0.50, na.rm = TRUE),
    IM_P75 = quantile(base$IDADEMAE, 0.75, na.rm = TRUE),
    
    IM_MD = mean(base$IDADEMAE, na.rm = TRUE),
    IM_DP = sd(base$IDADEMAE, na.rm = TRUE),
    
    # Escolaridade materna
    EM_S  = sum(base$ESCMAE2010 == "Sem escolaridade", na.rm = TRUE),
    EM_FI = sum(base$ESCMAE2010 == "Fundamental I", na.rm = TRUE),
    EM_FII = sum(base$ESCMAE2010 == "Fundamental II", na.rm = TRUE),
    EM_M  = sum(base$ESCMAE2010 == "Médio", na.rm = TRUE),
    EM_SI = sum(base$ESCMAE2010 == "Superior incompleto", na.rm = TRUE),
    EM_SC = sum(base$ESCMAE2010 == "Superior completo", na.rm = TRUE),
    
    # Raça/cor materna
    TGRC_B  = sum(base$RACACORMAE == "Branca", na.rm = TRUE),
    TGRC_PT = sum(base$RACACORMAE == "Preta", na.rm = TRUE),
    TGRC_A  = sum(base$RACACORMAE == "Amarela", na.rm = TRUE),
    TGRC_PD = sum(base$RACACORMAE == "Parda", na.rm = TRUE),
    TGRC_I  = sum(base$RACACORMAE == "Indígena", na.rm = TRUE),
    
    # Estado civil
    TGSC = sum(base$ESTCIV == "Sem companheiro", na.rm = TRUE),
    TGCC = sum(base$ESTCIV == "Com companheiro", na.rm = TRUE),
    
    # Paridade
    TGPRI  = sum(base$PARIDADE == "Primípara", na.rm = TRUE),
    TGNPRI = sum(base$PARIDADE == "Não primípara", na.rm = TRUE),

    # Gravidez
    TGU = sum(base$GRAVIDEZ == "Única", na.rm = TRUE),
    TGG = sum(base$GRAVIDEZ != "Única", na.rm = TRUE),

    # Duração da gestação
    TGD_22 = sum(base$GESTACAO == "<22 semanas", na.rm = TRUE),
    TGD_22_27 = sum(base$GESTACAO == "22 a 27 semanas", na.rm = TRUE),
    TGD_28_31 = sum(base$GESTACAO == "28 a 31 semanas", na.rm = TRUE),
    TGD_32_36 = sum(base$GESTACAO == "32 a 36 semanas", na.rm = TRUE),
    TGD_37_41 = sum(base$GESTACAO == "37 a 41 semanas", na.rm = TRUE),
    TGD_42 = sum(base$GESTACAO == "42 semanas e mais", na.rm = TRUE),

    TGD_PRT = sum(base$SEMAGESTAC < 37, na.rm = TRUE),
    TGD_AT  = sum(base$SEMAGESTAC >= 37 & base$SEMAGESTAC <= 41, na.rm = TRUE),
    TGD_PST = sum(base$SEMAGESTAC >= 42, na.rm = TRUE),

    DG_P25 = quantile(base$SEMAGESTAC, 0.25, na.rm = TRUE),
    DG_P50 = quantile(base$SEMAGESTAC, 0.50, na.rm = TRUE),
    DG_P75 = quantile(base$SEMAGESTAC, 0.75, na.rm = TRUE),
    DG_MD  = mean(base$SEMAGESTAC, na.rm = TRUE),
    DG_DP  = sd(base$SEMAGESTAC, na.rm = TRUE),

    # Pré-natal
    TKC_NR = sum(base$KOTELCHUCK == "Não realizou pré-natal", na.rm = TRUE),
    TKC_ID = sum(base$KOTELCHUCK == "Inadequado", na.rm = TRUE),
    TKC_IT = sum(base$KOTELCHUCK == "Intermediário", na.rm = TRUE),
    TKC_AD = sum(base$KOTELCHUCK == "Adequado", na.rm = TRUE),
    TKC_MAD = sum(base$KOTELCHUCK == "Mais que adequado", na.rm = TRUE),

    # Peregrinação
    TGPRG_S = sum(base$PERIG == "Sim", na.rm = TRUE),
    TGPRG_N = sum(base$PERIG == "Não", na.rm = TRUE),

    # Parto
    TPV = sum(base$PARTO == "Vaginal", na.rm = TRUE),
    TPC = sum(base$PARTO == "Cesáreo", na.rm = TRUE),

    # Apresentação
    TRAP_C = sum(base$TPAPRESENT == "Cefálica", na.rm = TRUE),
    TRAP_P = sum(base$TPAPRESENT == "Pélvica ou podálica", na.rm = TRUE),
    TRAP_T = sum(base$TPAPRESENT == "Transversa", na.rm = TRUE),
    
    # Robson
    TGROB_1 = sum(base$TPROBSON == "Grupo 1", na.rm = TRUE),
    TGROB_2 = sum(base$TPROBSON == "Grupo 2", na.rm = TRUE),
    TGROB_3  = sum(base$TPROBSON == "Grupo 3", na.rm = TRUE),
    TGROB_4  = sum(base$TPROBSON == "Grupo 4", na.rm = TRUE),
    TGROB_5  = sum(base$TPROBSON == "Grupo 5", na.rm = TRUE),
    TGROB_6  = sum(base$TPROBSON == "Grupo 6", na.rm = TRUE),
    TGROB_7  = sum(base$TPROBSON == "Grupo 7", na.rm = TRUE),
    TGROB_8  = sum(base$TPROBSON == "Grupo 8", na.rm = TRUE),
    TGROB_9  = sum(base$TPROBSON == "Grupo 9", na.rm = TRUE),
    TGROB_10 = sum(base$TPROBSON == "Grupo 10", na.rm = TRUE),
      
    # Local nascimento
    TNLOC_H = sum(base$LOCNASC == "Hospital", na.rm = TRUE),
    TNLOC_ES = sum(base$LOCNASC == "Outros estabelecimentos de saúde", na.rm = TRUE),
    TNLOC_D = sum(base$LOCNASC == "Domicílio", na.rm = TRUE),
    TNLOC_O = sum(base$LOCNASC == "Outros", na.rm = TRUE),
    TNLOC_AI = sum(base$LOCNASC == "Aldeia indígena", na.rm = TRUE),
    
    # Sexo
    TRS_M = sum(base$SEXO == "Masculino", na.rm = TRUE),
    TRS_F = sum(base$SEXO == "Feminino", na.rm = TRUE),
    
    # Raça
    TRRC_B  = sum(base$RACACOR == "Branca", na.rm = TRUE),
    TRRC_PT = sum(base$RACACOR == "Preta", na.rm = TRUE),
    TRRC_A  = sum(base$RACACOR == "Amarela", na.rm = TRUE),
    TRRC_PD = sum(base$RACACOR == "Parda", na.rm = TRUE),
    TRRC_I  = sum(base$RACACOR == "Indígena", na.rm = TRUE),
    
    # Peso
    TRP_BP = sum(base$F_PESO == "Baixo peso", na.rm = TRUE),
    TRP_N  = sum(base$F_PESO == "Peso normal", na.rm = TRUE),
    TRP_M  = sum(base$F_PESO == "Macrossomia", na.rm = TRUE),

    PESO_P25 = quantile(base$PESO, 0.25, na.rm = TRUE),
    PESO_P50 = quantile(base$PESO, 0.50, na.rm = TRUE),
    PESO_P75 = quantile(base$PESO, 0.75, na.rm = TRUE),
    PESO_MD  = mean(base$PESO, na.rm = TRUE),
    PESO_DP  = sd(base$PESO, na.rm = TRUE),

    # PIG
    TRPIG_P = sum(base$F_PIG == "PIG", na.rm = TRUE),
    TRPIG_A = sum(base$F_PIG == "AIG", na.rm = TRUE),
    TRPIG_G = sum(base$F_PIG == "GIG", na.rm = TRUE),

    # Apgar
    TRAPG5_B = sum(base$F_APGAR5 == "Baixo", na.rm = TRUE),
    TRAPG5_N = sum(base$F_APGAR5 == "Normal", na.rm = TRUE),

    APG5_MD = mean(base$APGAR5, na.rm = TRUE),
    APG5_DP = sd(base$APGAR5, na.rm = TRUE),

    # Anomalias
    TRAC = sum(base$IDANOMAL == "Sim", na.rm = TRUE),
    TRSAC = sum(base$IDANOMAL == "Não", na.rm = TRUE)
    
  )
}


linha_rj = resumo_sinasc(
  dados_sinasc_2,
  "UF",
  33
)

linha_rj$CODMUNRES = as.character(linha_rj$CODMUNRES)

# Linhas dos municípios
lista_municipios = split(
  dados_sinasc_2,
  dados_sinasc_2$CODMUNRES
)

linhas_municipios = bind_rows(
  lapply(names(lista_municipios), function(cod) {
    resumo_sinasc(
      lista_municipios[[cod]],
      "MUNICIPIO",
      cod
    )
  })
)


SINASC_RJ = bind_rows(
  linha_rj,
  linhas_municipios
)

write_csv(
  SINASC_RJ,
  "SINASC_RJ.csv"
)
