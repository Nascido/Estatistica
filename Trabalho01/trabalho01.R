library(readr)
library(doBy)
library(dplyr)

#00 Leitura e Tratamento de dados
stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")

# Exclusão de objeto na Base de Dados
stroke_data = stroke_data[stroke_data$gender!="Other",]

# Mudança de status das variáveis
stroke_data$hypertension = case_match(stroke_data$hypertension, 0~"Não", 1~"Sim")
stroke_data$heart_disease = case_match(stroke_data$heart_disease, 0~"Não", 1~"Sim")
stroke_data$ever_married = case_match(stroke_data$ever_married, "No"~"Não", "Yes"~"Sim")
stroke_data$Residence_type = case_match(stroke_data$Residence_type, "Urban"~"Urbano", "Rural"~"Rural")
stroke_data$stroke = case_match(stroke_data$stroke, 0~"Não", 1~"Sim")
stroke_data$gender = case_match(stroke_data$gender, "Female"~"Feminino", "Male"~"Masculino")
stroke_data$smoking_status = case_match(stroke_data$smoking_status, "formerly smoked"~"Fumou",
                                        "never smoked"~"Nunca Fumou",
                                        "smokes"~"Fuma", "Unknown"~"S/ info")
# Remoção da Coluna work_type
stroke_data = select(stroke_data, -work_type)

# Mudança de tipagem da variavel bmi

## Fuções para cálculo da média, desvio padrão e coeficiente de variação
cof_var <- function(values){
  return(100*sd(values)/mean(values))
}
avaliacao <- function(x){
  c(media=mean(x), desvio=sd(x), cof=cof_var(x))
}

#01 Analise da quantidade de observações para cada variável quantitativa
table(stroke_data$stroke)

table(stroke_data$gender)

table(stroke_data$hypertension)

table(stroke_data$heart_disease)

table(stroke_data$ever_married)

table(stroke_data$smoking_status)

table(stroke_data$work_type)

table(stroke_data$Residence_type)


#Gráficos de Entendimento

boxplot(age~stroke,
        data = stroke_data)

boxplot(age~smoking_status,
        data = stroke_data)

boxplot(avg_glucose_level~stroke,
        data = stroke_data)

boxplot()
