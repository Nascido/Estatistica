
library(readr)
library(doBy)
library(dplyr)

################################################################################
##00 Tratamento de Dados #######################################################
################################################################################

## Leitura da Base de Dados
stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

## Exclusão de objeto na Base de Dados
stroke_data = stroke_data[stroke_data$gender!="Other",]

## Mudança de status das variáveis
stroke_data$hypertension = case_match(stroke_data$hypertension, 0~"Não", 1~"Sim")
stroke_data$heart_disease = case_match(stroke_data$heart_disease, 0~"Não", 1~"Sim")
stroke_data$stroke = case_match(stroke_data$stroke, 0~"Não", 1~"Sim")
stroke_data$gender = case_match(stroke_data$gender, "Female"~"Feminino", "Male"~"Masculino")
stroke_data$smoking_status = case_match(stroke_data$smoking_status, "formerly smoked"~"Fumou",
                                        "never smoked"~"Nunca Fumou",
                                        "smokes"~"Fuma", "Unknown"~"S/ info")


## Remoção de Colunas não usadas
stroke_data = select(stroke_data, -work_type)
stroke_data = select(stroke_data, -ever_married)
stroke_data = select(stroke_data, -Residence_type)

## Mudança de tipagem da variavel bmi - chr --> float
stroke_data = stroke_data %>%
  mutate(imc = case_when(bmi == "N/A" ~ -1, 
                         .default = as.numeric(bmi)))

stroke_data = select(stroke_data, -bmi)


################################################################################
##01 Análise de Dados ##########################################################
################################################################################

################################################################################
# Funções Úteis Criadas

## Criação de uma Tabela de apenas uma variável
one_var_table <- function(data_var, name_var){
  ftabela = table(data_var, useNA = "ifany") 
  ptabela = round(prop.table(ftabela)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-3]
  colnames(tabela) <- c(name_var,"Frequencia","Porcentagem")
  
  return(tabela)
}

## Criação de uma Tabela de duas variáveis
two_var_table <- function(var1, var2, name1, name2){
  ftabela = table(var1, var2, useNA = "ifany") 
  ptabela = round(prop.table(ftabela,1)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-c(4,5)]
  colnames(tabela) <- c(name1,name2,"Frequencia","Porcentagem")
  
  return(tabela)
}

## Criação de uma Tabela relacionando uma variável à variável AVC
stroke_var_table <- function(data_var, name_var){
  tabela = two_var_table(data_var, stroke_data$stroke, name_var, "AVC")
  return(tabela)
}


################################################################################
# TABELAS PARA VARIAVEIS QUALITATIVAS:

## Sexo:
tabela_gender = one_var_table(stroke_data$gender, "Sexo")
tabela_gender_stroke = stroke_var_table(stroke_data$gender, "Sexo")

## Hipertensão
tabela_hypertension = one_var_table(stroke_data$hypertension, "Hipertensão")
tabela_hypertension_stroke = stroke_var_table(stroke_data$hypertension, "Hipertensão")

## Problema no Coração
tabela_heart = one_var_table(stroke_data$heart_disease, "Cardiaco")
tabela_heart_stroke = stroke_var_table(stroke_data$heart_disease, "Cardiaco")

## Fumante
tabela_smoke = one_var_table(stroke_data$smoking_status, "Cigarro")
tabela_smoke_stroke = stroke_var_table(stroke_data$smoking_status, "Cigarro")

## AVC
tabela_stroke = one_var_table(stroke_data$stroke, "AVC")

#################################################################################
# MEDIDAS DE RESUMO

## Idade
summary(stroke_data$age)

## Glicose no Sangue
summary(stroke_data$avg_glucose_level)

## IMC
summary(stroke_data$imc[stroke_data$imc != -1])

#################################################################################
# GRAFICOS DAS VARIAVEIS DE ESTUDO

## Gênero
library(ggplot2)
ggplot(stroke_data, aes(x=gender)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Gênero", y="Contagem")

ggplot(stroke_data, aes(x=gender, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Gênero") +
  ylab("Porcentagem")

## Idade
ggplot(stroke_data, aes(x=age)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Idade", y="Contagem")

boxplot(age~stroke,
        data = stroke_data,
        main = "AVC em relação a idade",
        xlab = "Idade",
        ylab = "AVC",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,100))

## Hipertensão
ggplot(stroke_data, aes(x=hypertension)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem hipertensão", y="Contagem")

ggplot(stroke_data, aes(x=hypertension, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Hipertensão") +
  ylab("Porcentagem")

## Problema de coração
ggplot(stroke_data, aes(x=heart_disease)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem problema de coração", y="Contagem")

ggplot(stroke_data, aes(x=heart_disease, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Problema de coração") +
  ylab("Porcentagem") 

## Nível médio de glicose
ggplot(stroke_data, aes(x=avg_glucose_level)) +
  geom_histogram(binwidth=5, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Nível de glicose médio", y="Contagem")

boxplot(avg_glucose_level~stroke,
        data = stroke_data,
        main = "AVC em relação ao nível de médio de glicose",
        xlab = "Nível médio de glicose ",
        ylab = "AVC",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,300))

## Status de fumante
ggplot(stroke_data, aes(x=smoking_status)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Status de fumante", y="Contagem")

ggplot(stroke_data, aes(x=smoking_status, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Status de fumante") +
  ylab("Porcentagem")

## AVC
ggplot(stroke_data, aes(x=stroke)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Teve AVC", y="Contagem")

## IMC
ggplot(stroke_data, aes(x=imc)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="IMC", y="Contagem")

stroke_data_filtered = stroke_data[stroke_data$imc != -1,]
boxplot(imc~stroke,
        data = stroke_data_filtered,
        main = "AVC em relação ao IMC",
        xlab = "Nível médio de glicose ",
        ylab = "IMC",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,100))
