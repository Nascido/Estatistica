
library(readr)
library(doBy)
library(dplyr)

################################################################################
##00 Tratamento de Dados #######################################################
################################################################################

#Leitura da Base de Dados
stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

#Exclusão de objeto na Base de Dados
stroke_data = stroke_data[stroke_data$gender!="Other",]

#Mudança de status das variáveis
stroke_data$hypertension = case_match(stroke_data$hypertension, 0~"Não", 1~"Sim")
stroke_data$heart_disease = case_match(stroke_data$heart_disease, 0~"Não", 1~"Sim")
stroke_data$ever_married = case_match(stroke_data$ever_married, "No"~"Não", "Yes"~"Sim")
stroke_data$Residence_type = case_match(stroke_data$Residence_type, "Urban"~"Urbano", "Rural"~"Rural")
stroke_data$stroke = case_match(stroke_data$stroke, 0~"Não", 1~"Sim")
stroke_data$gender = case_match(stroke_data$gender, "Female"~"Feminino", "Male"~"Masculino")
stroke_data$smoking_status = case_match(stroke_data$smoking_status, "formerly smoked"~"Fumou",
                                        "never smoked"~"Nunca Fumou",
                                        "smokes"~"Fuma", "Unknown"~"S/ info")


#Remoção da Coluna work_type
stroke_data = select(stroke_data, -work_type)

#Mudança de tipagem da variavel bmi - chr --> float
stroke_data = stroke_data %>%
  mutate(imc = case_when(bmi == "N/A" ~ -1, 
                         .default = as.numeric(bmi)))

stroke_data = select(stroke_data, -bmi)


################################################################################
##01 Análise de Dados ##########################################################
################################################################################

################################################################################
# Funções Úteis Criadas

one_var_table <- function(data_var, name_var){
  ftabela = table(data_var, useNA = "ifany") 
  ptabela = round(prop.table(ftabela)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-3]
  colnames(tabela) <- c(name_var,"Frequencia","Porcentagem")
  
  return(tabela)
}

two_var_table <- function(var1, var2, name1, name2){
  ftabela = table(var1, var2, useNA = "ifany") 
  ptabela = round(prop.table(ftabela,1)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-c(4,5)]
  colnames(tabela) <- c(name1,name2,"Frequencia","Porcentagem")
  
  return(tabela)
}

stroke_var_table <- function(data_var, name_var){
  tabela = two_var_table(data_var, stroke_data$stroke, name_var, "AVC")
  return(tabela)
}


################################################################################
# TABELAS PARA VARIAVEIS QUALITATIVAS:

## Variavel Sexo:
tabela_gender = one_var_table(stroke_data$gender, "Sexo")
tabela_gender_stroke = stroke_var_table(stroke_data$gender, "Sexo")

## Variável Hipertensão
tabela_hypertension = one_var_table(stroke_data$hypertension, "Hipertensão")
tabela_hypertension_stroke = stroke_var_table(stroke_data$hypertension, "Hipertensão")

## Variável Problema no Coração
tabela_heart = one_var_table(stroke_data$heart_disease, "Cardiaco")
tabela_heart_stroke = stroke_var_table(stroke_data$heart_disease, "Cardiaco")

## Variável Casado
tabela_married = one_var_table(stroke_data$ever_married, "Casado")
tabela_married_stroke = stroke_var_table(stroke_data$ever_married, "Casado")

## Variável Residencia
tabela_residence = one_var_table(stroke_data$Residence_type, "Residencia")
tabela_residence_stroke =  stroke_var_table(stroke_data$Residence_type, "Residencia")

## Variavel Fumante
tabela_smoke = one_var_table(stroke_data$smoking_status, "Cigarro")
tabela_smoke_stroke = stroke_var_table(stroke_data$smoking_status, "Cigarro")

## Variavel AVC
tabela_stroke = one_var_table(stroke_data$stroke, "AVC")

#################################################################################
# MEDIDAS DE RESUMO

## Idade
summary(stroke_data$age)

## Glicose no Sangue
summary(stroke_data$avg_glucose_level)

## IMC
summary(stroke_data$imc[stroke_data$imc != -1])
