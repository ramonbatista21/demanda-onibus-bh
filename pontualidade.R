
#Verificando Diretorio de Trabalho
getwd()

#Importando bibliotecas
library('readxl') #ler excel
library('dplyr') #Manipulação
library('lubridate') #datas
library('ggplot2')
library('gridExtra')
library('corrplot')
library('MASS')

#Importando os dados
df <- read_excel('BD_Pont.xlsx')

#Visualizando a estrutura dos dados
View(df)
str(df)

#Explorando as variáveis
summary(df)

#Selecionando as variáveis 
df <- df[c('Demanda', 'Minutos Atraso Pontualidade', 'TP VG')]
head(df)

#Renomeando Colunas
df <- rename(df, demanda = Demanda, atraso = `Minutos Atraso Pontualidade`, tempo = `TP VG`)
df

#Verificando valores nulos
sum(is.na(df))
df <- na.omit(df)  #Removendo valores na
sum(is.na(df))

#Converter tempo para minutos
t <- ymd_hms(df$tempo)
df$tempo <- (hour(t)*60 + minute(t)+ second(t)/360)
head(df)

summary(df)


#Criando Gráficos
g1 <- ggplot(df, aes(y = demanda, x = atraso)) +
  geom_point() +
  xlab("Atraso (min)") + 
  ylab("Demanda")

g2 <- ggplot(df, aes(y = demanda, x = tempo)) +
  geom_point() +
  xlab("Tempo de Viagem (min)") + 
  ylab("Demanda") 

g3 <-ggplot(df, aes(y = atraso, x = tempo)) +
  geom_point() +
  xlab("Tempo de Viagem") + 
  ylab("Atraso (min)") 

grid.arrange(g1, g2, g3)

g4 <- ggplot(df, aes(y = demanda)) +
  geom_boxplot(color = "#393c54", fill = "lightblue") +
  theme_classic(base_size = 8)+
  xlab("Demanda")+
  ylab("")

g5 <- ggplot(df, aes(y = atraso)) +
  geom_boxplot(color = "#393c54", fill = "lightblue") +
  theme_classic(base_size = 8)+
  xlab("Atraso (min)")+
  ylab("")

g6 <- ggplot(df, aes(y = tempo)) +
  geom_boxplot(color = "#393c54", fill = "lightblue")  +
  theme_classic(base_size = 8)+
  xlab("Tempo (min)")+
  ylab("")

grid.arrange(g4, g5, g6, ncol=3)

g7 <- ggplot(df, aes(x = demanda)) +
  geom_histogram(color = "white", fill = "lightblue", bins=10)  +
  theme_classic(base_size = 8)+
  xlab("Demanda")+
  ylab("Frequência")

g8 <- ggplot(df, aes(x = atraso)) +
  geom_histogram(color = "white", fill = "lightblue", bins=10) +
  theme_classic(base_size = 8)+
  scale_x_continuous(breaks = seq(from = 0,to = 20,by = 10), limits = c(0,20)) +
  xlab("Atraso (min)")+
  ylab("Frequência")


g9 <- ggplot(df, aes(x = tempo)) +
  geom_histogram(color = "white", fill = "lightblue", bins=10) +
  theme_classic(base_size = 8)+
  scale_x_continuous(breaks = seq(from = 0,to = 200,by = 30), limits = c(0,200))+
  xlab("Tempo (min)")+
  ylab("Frequência")

grid.arrange(g7, g8, g9, ncol=3)

#Eliminando Outliers
outliers <- boxplot(df$atraso, plot=FALSE)$out
df <- df[-which(df$atraso %in% outliers),]

outliers <- boxplot(df$tempo, plot=FALSE)$out
df <- df[-which(df$tempo %in% outliers),]

#Repetindo gráficos
g4 <- ggplot(df, aes(y = demanda)) +
  geom_boxplot(color = "#393c54", fill = "lightblue") +
  theme_classic(base_size = 8)+
  xlab("Demanda")+
  ylab("")

g5 <- ggplot(df, aes(y = atraso)) +
  geom_boxplot(color = "#393c54", fill = "lightblue") +
  theme_classic(base_size = 8)+
  xlab("Atraso (min)")+
  ylab("")

g6 <- ggplot(df, aes(y = tempo)) +
  geom_boxplot(color = "#393c54", fill = "lightblue")  +
  theme_classic(base_size = 8)+
  xlab("Tempo (min)")+
  ylab("")

grid.arrange(g4, g5, g6, ncol=3)

#Summary
summary(df)

#Correlações
cor.df <- cor(df, method = "spearman")
cor(df)

#Nota-se que os valores são próximos de 0, não existindo correlações estatísticas entre as variáveis.

#Regressão 
modelo1 <- lm(df$atraso~df$demanda)
summary(modelo1)

modelo2 <- lm(df$tempo~df$demanda)
summary(modelo2)

modelo3 <- lm(df$atraso~df$tempo)
summary(modelo3)


#Teste T
t.test(df$atraso,df$demanda)
t.test(df$tempo,df$demanda)
t.test(df$atraso,df$tempo)

#IC
confint(modelo1, level=0.95)
confint(modelo2, level=0.95)
confint(modelo3, level=0.95)


# H0 = Dependência entre as variáveis
# H1 = Hipotese alternativa 
# ?? = 0,05 

#Teste Qui-Quadrado
chisq.test(df)
chisq.test(df$atraso,df$demanda)
chisq.test(df$tempo,df$demanda)
chisq.test(df$atraso,df$tempo)

#Percebe-se que em todos os casos o p-value é praticamente zero
#Portanto, rejeita-se a hipótese de existência de dependência entre as variáveis