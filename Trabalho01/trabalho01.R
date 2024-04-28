
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

#Cálculo medidas de resumo
summary(stroke_data$age)
summary(stroke_data$avg_glucose_level)
summary(stroke_data$imc[stroke_data$imc != -1])
