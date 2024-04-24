library(readr)
library(doBy)

#00 Leitura de dados
stroke_data <- read_csv("healthcare-dataset-stroke-data.csv")


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
