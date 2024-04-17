#0 Leitura da base de dados e amostra aleatória simples de 600 celulares
dados <- read.csv2("mobile.csv", dec=".")

library("dplyr")

set.seed("12345")
tam_amostra = 600

amostra = sample_n(dados, tam_amostra)

## Função para cálculo do coeficiente de variação
cof_var <- function(values){
  return(100*sd(values)/mean(values))
}

###########################################################################################
#1 Calcule a média, desvio padrão e coeficiente de variação para as seguintes variáveis: ##
###########################################################################################

## (a)  “battery_power”, em função das categorias da variável “touch_screen”;
by(amostra$battery_power, amostra$touch_screen, mean)
by(amostra$battery_power, amostra$touch_screen, sd)
by(amostra$battery_power, amostra$touch_screen, cof_var)

## (b)  “m_dep”, em função das categorias da variável “touch_screen”;
by(amostra$m_dep, amostra$touch_screen, mean)
by(amostra$m_dep, amostra$touch_screen, sd)
by(amostra$m_dep, amostra$touch_screen, cof_var)


## (c)  “int_memory”, em função das categorias da variável “blue”.
by(amostra$int_memory, amostra$blue, mean)
by(amostra$int_memory, amostra$blue, sd)
by(amostra$int_memory, amostra$blue, cof_var)


###########################################################################################
#2 Faça os gráficos de caixa das análises dos itens 1a, 1b, 1c e tire conclusões. #########
###########################################################################################

## Mudança de valores de variáveis para melhor interpretação
amostra$touch_screen = case_match(amostra$touch_screen, 0~"Sem Touch", 1~"Com Touch")
amostra$blue = case_match(amostra$blue, 0~"Sem Bluetooth", 1~"Com Bluetooth")

## (a)  Gráfico de Caixa: “battery_power” x “touch_screen”;
boxplot(battery_power~touch_screen,
        data = amostra,
        main = "Comparação de Bateria: Celular com e sem Touch",
        xlab = "Touch Screen",
        ylab = "Capacidade da Bateria ( mAh )",
        col  = c("lightgreen","pink"),
        ylim = c(200, 2200))

## (b)  Gráfico de Caixa: “m_dep” x “touch_screen”;
boxplot(m_dep~touch_screen,
        data = amostra,
        main = "Comparação de Profundidade: Celular com e sem Touch",
        xlab = "Touch Screen",
        ylab = "Profundidade do Celular ( cm )",
        col  = c("lightgreen","pink"),
        ylim = c(0.0, 1.2))


## (c)  Gráfico de Caixa: “int_memory” x “blue”.
boxplot(int_memory~blue,
        data = amostra,
        main = "Comparação de Memória Interna: Celular com e sem Bluetooth",
        xlab = "Bluetooth",
        ylab = "Memória Interna ( GB )",
        col  = c("lightblue","lightyellow"),
        ylim = c(0,70))
