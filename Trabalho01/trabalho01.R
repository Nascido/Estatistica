
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

#Gráficos
#Gênero
library(ggplot2)
ggplot(stroke_data, aes(x=gender)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Gênero", y="Contagem")

#Idade
ggplot(stroke_data, aes(x=age)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Idade", y="Contagem")

#Hipertensão
ggplot(stroke_data, aes(x=hypertension)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem hipertensão", y="Contagem")

#Problema de coração
ggplot(stroke_data, aes(x=heart_disease)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tem problema de coração", y="Contagem")

#Já foi casado(a)
ggplot(stroke_data, aes(x=ever_married)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Já foi casado(a)", y="Contagem")

#Tipo de residência
ggplot(stroke_data, aes(x=Residence_type)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Tipo de residência", y="Contagem")

#Nível médio de glicose
ggplot(stroke_data, aes(x=avg_glucose_level)) +
  geom_histogram(binwidth=5, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="Nível de glicose médio", y="Contagem")

#Status de fumante
ggplot(stroke_data, aes(x=smoking_status)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Status de fumante", y="Contagem")

#AVC
ggplot(stroke_data, aes(x=stroke)) +
  geom_bar(fill="lightblue", color="blue", alpha=0.8) +
  labs(x="Teve AVC", y="Contagem")

#IMC
ggplot(stroke_data, aes(x=imc)) +
  geom_histogram(binwidth=1, alpha=0.7,  color="blue", fill="lightblue") + 
  labs(x="IMC", y="Contagem")

