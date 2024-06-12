
library(readr)
library(doBy)
library(dplyr)
library(ggplot2)

################################################################################
##00 Tratamento de Dados #######################################################
################################################################################

## Leitura da Base de Dados
stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

## Remoção de Colunas não usadas
stroke_data = select(stroke_data, -work_type)
stroke_data = select(stroke_data, -ever_married)
stroke_data = select(stroke_data, -Residence_type)

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

## Mudança de tipagem da variavel IMC : chr --> float
stroke_data = stroke_data %>%
  mutate(imc = case_when(bmi == "N/A" ~ -1, 
                         .default = as.numeric(bmi)))

stroke_data = select(stroke_data, -bmi)

################################################################################
##01 Análise de Dados ##########################################################
################################################################################

################################################################################
# Funções Úteis Criadas:

## Criação de uma Tabela de apenas uma variável
one_var_table <- function(data_var, name_var){
  ftabela = table(data_var, useNA = "ifany") 
  ptabela = round(prop.table(ftabela)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-3]
  colnames(tabela) <- c(name_var,"Frequencia","Porcentagem")
  
  print(tabela)
  
  return(tabela)
}

## Criação de uma Tabela de duas variáveis
two_var_table <- function(var1, var2, name1, name2){
  ftabela = table(var1, var2, useNA = "ifany") 
  ptabela = round(prop.table(ftabela,1)*100,1)
  tabela = data.frame(ftabela,ptabela)
  tabela = tabela[,-c(4,5)]
  colnames(tabela) <- c(name1,name2,"Frequencia","Porcentagem")
  
  print(tabela)
  
  return(tabela)
}

## Criação de uma Tabela relacionando uma variável à variável AVC
stroke_var_table <- function(data_var, name_var){
  tabela = two_var_table(data_var, stroke_data$stroke, name_var, "AVC")
  
  return(tabela)
}

#################################################################################
# MEDIDAS DE RESUMO PARA QUANTITATIVAS:

## Idade
summary(stroke_data$age)

## Nível médio de glicose
summary(stroke_data$avg_glucose_level)

## IMC
summary(stroke_data$imc[stroke_data$imc != -1])

################################################################################
# TABELAS PARA VARIAVEIS QUALITATIVAS:

## Gênero:
tabela_gender = one_var_table(stroke_data$gender, "Gênero")
tabela_gender_stroke = stroke_var_table(stroke_data$gender, "Gênero")

## Hipertensão
tabela_hypertension = one_var_table(stroke_data$hypertension, "Hipertensão")
tabela_hypertension_stroke = stroke_var_table(stroke_data$hypertension, "Hipertensão")

## Problema no Coração
tabela_heart = one_var_table(stroke_data$heart_disease, "Cardiaco")
tabela_heart_stroke = stroke_var_table(stroke_data$heart_disease, "Cardiaco")

## Histórico de fumante
tabela_smoke = one_var_table(stroke_data$smoking_status, "Cigarro")
tabela_smoke_stroke = stroke_var_table(stroke_data$smoking_status, "Cigarro")

## AVC
tabela_stroke = one_var_table(stroke_data$stroke, "AVC")

#################################################################################
# ALTERAÇÃO DO TAMANHO DA FONTE DOS GRÁFICOS DE CAIXA
par(cex.axis = 1.7, cex.lab = 1.7)

#################################################################################
# GRAFICOS DAS VARIAVEIS DE ESTUDO:

## Idade
ggplot(stroke_data, aes(x=age)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Idade (anos)", y="Contagem") +
  theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20))

boxplot(age~stroke,
        data = stroke_data,
        xlab = "AVC",
        ylab = "Idade (anos)",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,100))

## Nível médio de glicose
ggplot(stroke_data, aes(x=avg_glucose_level)) +
  geom_histogram(binwidth=5, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Nível de glicose médio (mg/dL)", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

boxplot(avg_glucose_level~stroke,
        data = stroke_data,
        xlab = "AVC",
        ylab = "Nível médio de glicose (mg/dL)",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,300))

## IMC
ggplot(stroke_data, aes(x=imc)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="IMC (kg/m2)", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

stroke_data_filtered = stroke_data[stroke_data$imc != -1,]
boxplot(imc~stroke,
        data = stroke_data_filtered,
        xlab = "AVC",
        ylab = "IMC (kg/m2)",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,100))

## Gênero
ggplot(stroke_data, aes(x=gender)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Gênero", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(stroke_data, aes(x=gender, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Gênero") +
  ylab("Porcentagem") + 
  labs(fill="AVC") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

## Hipertensão
ggplot(stroke_data, aes(x=hypertension)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem hipertensão", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(stroke_data, aes(x=hypertension, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Hipertensão") +
  ylab("Porcentagem") + 
  labs(fill="AVC") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

## Problema de coração
ggplot(stroke_data, aes(x=heart_disease)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem problema de coração", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(stroke_data, aes(x=heart_disease, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Problema de coração") +
  ylab("Porcentagem") + 
  labs(fill="AVC") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

## Histórico de fumante
ggplot(stroke_data, aes(x=smoking_status)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Histórico de fumante", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

ggplot(stroke_data, aes(x=smoking_status, fill=stroke)) + 
  geom_bar(position="fill") +
  xlab("Histórico de fumante") +
  ylab("Porcentagem") + 
  labs(fill="AVC") +
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))

## AVC
ggplot(stroke_data, aes(x=stroke)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Teve AVC", y="Contagem") + 
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

