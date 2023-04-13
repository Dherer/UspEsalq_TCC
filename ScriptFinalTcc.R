#### analises tcc ####

library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(janitor)
library(ggpubr)
library(writexl)
library(lubridate)
library(gt)
library(glue)
library(reactable)
library(flextable)
library(gtsummary)
library(jtools)
library(ggstance)
library(huxtable)
library(Hmisc)
library(plotrix)
library(car)
library(broom.mixed)
library(ggiraphExtra)
library(scales)
library(visreg)

#### Importando bases de dados ####
setwd("D:/Lucas/TCC - MBA/DadosCerrado") #indicando diretório de trabalho

dir() #mostra todos os arquivos que tem no diretorio

#Base focos de queimada
queimadas <- read.csv("historico_bioma_cerrado_focos_de_queimada.csv") 

#Base PAM
pam <- read_excel("PAM_tabela5457.xlsx")

#Base PPM
ppm <- read_excel("PPM_tabela3939.xlsx")

#Cobertura vegetal (y)
coberturavegetal <- read.csv("area_total_biomas_cerrado.csv")

#Lista de municipios do cerrado 
municipioscerrado <- read_excel("Lista_Municipio_Bioma.xls")


#### Manipulando as data frames ####

#Editando a base queimadas

colnames(queimadas)[1] <- "Ano" #nomeando a coluna 1 de "x" para "Ano"
queimadas <- queimadas[-(26:28),] #excluindo as linhas 26, 27 e 28 - valores de max, med e min por coluna
queimadas[queimadas=="-"] <- NA #transformando o hifens em NA, para entender que sao valores faltantes

#Ediantando a base da PAM

nomes_colunas_pam <- as.character(pam[3,]) #criando um vetor para o nome das colunas
pam <- pam[-(1:4),] #excluindo as linhas 1 a 4 (legendas de nomes de coluna)
pam <- pam[-5564,] #excluindo a ultima linha (fonte IBGE)
colnames(pam) <- nomes_colunas_pam #renomeando as colunas do PAM
pam[pam=="..."] <- NA #transformando as reticencias em NA, para entender que são valores faltantes
colnames(pam)[1] <- "CD_GEOCMU" #transformando o nome da primeira coluna de PAM, para poder filtrar mais a frente por cidade do cerrado

#Eidtando a Base PPM

tiporebanho <- ppm[4,] #salvando os nomes de tipos de rebanho em um vetor
tiporebanho <- as.character(tiporebanho) #transformando o vetor em caracter
tiporebanho <- tiporebanho[-(1:2)] #tirando os NAs do vetor
ppmano <- rep(1974:2020,each=4) #criando um vetor de anos de 1974 ate 2020, repetindo a cada 4x pois existem quatro tipos de rebanho
ppm <- ppm[-(1:5),]  #retirando as duas primeiras linhas, pois são linhas de legendas de variaveis
ppm[ppm=="..."] <- NA #transforma em NA as reticencias para indicar dado faltando
ppm[ppm=="-"] <- NA #transforma em vazio os hifens
colnames(ppm) <- c("CD_GEOCMU","municipio",ppmano) #renomando os nomes das colunas de ppm, para poder filtrar mais a frente por cidade do cerrado

#Criando um data frame com municipios do cerrado

municipioscerrado <- municipioscerrado %>%
  arrange(BIOMA) %>%
  filter(BIOMA=="Cerrado") #filtrando apenas os municipios do cerrado

#### Unindo bases de dados #####

#Criando a coluna de cobertura vegetal por ano

datafloresta  <- 1985:2020 #criando um vetor de anos
datafloresta  <- as.character(datafloresta) #transformando o vetor em character
totalfloresta <- as.data.frame(t(coberturavegetal[1,])) #fazendo a transposta de cobertura vegetal
totalfloresta <- tibble::rownames_to_column(totalfloresta,"ano") #transformando os nomes das linhas em uma coluna
totalfloresta$ano <- substr(totalfloresta$ano,2,5) #ajeita o nome da coluna ano, mantendo do caracter 2 ao caracter 5
names(totalfloresta)[names(totalfloresta) == "1"] <- "hectares_cerrado" #renomeando a coluna "coberetura vegetal" para "hectares_cerrado"
totalfloresta <- totalfloresta[-(1:14),] #retirando as linhas 1 a 14

#Criando a coluna com o total de queimadas por ano

totalqueimadas <- queimadas%>%
  select(Ano,Total)%>%
  filter(Ano<=2020) #filtrando pelos anos menores ou iguais a 2020 de queimada
names(totalqueimadas)[names(totalqueimadas) == "Total"] <- "total_focos_queimadas" #renomeando a coluna de total 

#Criando a coluna com o total de producao por ano, levando em consideracao municipios

totalagro <- merge(pam,municipioscerrado, by="CD_GEOCMU") #filtrando apenas os valores referentes as cidades do cerrado

totalagro <- totalagro%>%
             select(-(CD_GEOCMU))%>%
             select(-(NM_MUNICIP))%>%
             select(-(BIOMA)) #retirando as colunas desnecessarias, deixando apenas os anos

data <- colnames(totalagro) #criando um vetor com os nomes das colunas
data <- data[-1] #retirando o primeiro item do vetor ("municipio")
totalagro <- t(totalagro) #fazendo a transposta de totalagro
colnames(totalagro) <- totalagro[1,] #reanomeando as linhas com os nomes das colunas
totalagro <- totalagro[-(1),]  #retirando a primeira linha com nomes de municipios
totalagro <- as.data.frame(totalagro) #transformando o objeto totalagro de matriz para DF

totalagro <- totalagro %>%
             mutate_if(is.character, as.numeric) #transformando os dados de caractere em numerico

totalagro$total_ha_plantado <- apply(totalagro, 1, sum, na.rm = T) #adicionando uma coluna com as somas das linhas, descosiderando os ND's
totalagro$ano <- data #adiciona a coluna de ano               

totalagro <- totalagro%>%
             select(ano, total_ha_plantado)%>%
             filter(ano>=1998) #selecioanando apenas as variaveis de interesse e filtrando a partir de 1998

#Criando coluna com o total de producao da pecuaria

totalboi <- merge(ppm,municipioscerrado, by="CD_GEOCMU") #Unindo os DF contendo apenas os valores referentes ao cerrado
totalboi <- totalboi[,-c(2,191,192)] #excluindo colunas 2, 191 e 192
totalboi <- t(totalboi) #fazendo a transposta de totalboi
totalboi <- as.data.frame(totalboi) #transformando o objeto em data frame
totalboi <- totalboi[-1,] #excluindo a primeira linha que contem os codigos das cidades
totalboi <- tibble::rownames_to_column(totalboi,"ano") #transformando os nomes das linhas em uma coluna
totalboi <- data.frame(lapply(totalboi, function(x) as.numeric(as.character(x)))) #transformando aquilo que é caracter em numerico
totalboi <- totalboi[,-1] #excluindo a primeira coluna
totalboi$total <- apply(totalboi, 1, sum, na.rm = T) #fazendo a coluna com os totais 
totalboi$ano <- ppmano #adicionando a coluna de anos
totalboi$tipo_rebanho <- tiporebanho #adicionando os tipos de rebanho

totalboi <- totalboi %>%
            select(ano, tipo_rebanho, total)%>%
            filter(ano>=1998) #filtrando os anos a partir de 1998

totalboi <- cbind(subset(totalboi,tipo_rebanho == "Bovino"),
                  subset(totalboi,tipo_rebanho == "Bubalino"),
                  subset(totalboi,tipo_rebanho == "Suíno - matrizes de suínos"),
                  subset(totalboi,tipo_rebanho == "Galináceos - galinhas")) #juntando as colunas 

names(totalboi) <- c("ano","X1","bovino",
                     "ano1","X2","bubalino",
                     "ano2","X3","suino",
                     "ano3","X4","galinaceos") #renomeando as colunas do tipo de rebanho

totalboi <- subset(totalboi,select=-c(ano1,ano2,ano3,X1,X2,X3,X4)) #retirando colunas

#### Construindo o df para realizar as analises #####

dadostcc <- cbind(totalfloresta, totalqueimadas, totalagro, totalboi) #juntando as variaveis de analise em um df
dadostcc <- dadostcc[,-c(3,5,7)] #retirando as colunas 3, 5, e 7
dadostcc$total_focos_queimadas <- as.numeric(dadostcc$total_focos_queimadas) #transformando a variavel em numericas
dadostcc$hectares_cerrado <- as.numeric(dadostcc$hectares_cerrado) #transformando a variavel em numeric
dadostcc$ano <- as.factor(dadostcc$ano) #transformando o ano em fator
dadostcc$ano <- as.Date(dadostcc$ano, format ="%Y") #transformar a variável do ano em data
dadostcc$ano <- year(dadostcc$ano) #extrair apenas o valor de ano
dadostcc[dadostcc == 0] <- NA #transformando os zeros em NA

#### realizando analise exploratoria dos dados #####

str(dadostcc) #visualiza a estrutura do df
summary(dadostcc) #apresentando o resumo descritivo do df
glimpse(dadostcc) #outra possibilidade de resumo dos anos

#obtendo estatisticas descritivas dos dados

dadostcc_resumo <- dadostcc %>% #seleciona o df de dados tcc
                   select(-ano) %>% #seleciona todas as variaveis menos a do ano
                   na.omit() %>% #retira os NA das analises
                   summarise_all(list(min=min, max=max, mean=mean, median=median, sd=sd)) #traz a media, max, mini...


#visualizando variaveis em relação ao ano

anocerrado <- ggplot(dadostcc, aes(x=ano)) +
              geom_line(aes(y=hectares_cerrado)) +
              xlab("Ano") +
              ylab("Cobertura Vegetal") +
              scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
              theme_bw()

anoagro <- ggplot(dadostcc, aes(x=ano)) +
           geom_line(aes(y=total_ha_plantado)) +
           xlab("Ano") +
           ylab("Área Plantada") +
           scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
           theme_bw()

anoqueimada <- ggplot(dadostcc, aes(x=ano)) +
               geom_line(aes(y=total_focos_queimadas)) +
               xlab("Ano") +
               ylab("Focos de Queimada") +
               scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
               theme_bw()

anobovino <- ggplot(dadostcc, aes(x=ano)) +
             geom_line(aes(y=bovino)) +
             xlab("Ano") +
             ylab("População de Bovinos") +
             scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
             theme_bw()

anosuinos <- ggplot(dadostcc, aes(x=ano, na.rm = TRUE)) +
             geom_line(aes(y=suino)) +
             xlab("Ano") +
             ylab("População de Suínos") +
             scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
             theme_bw()

anobubalinos <- ggplot(dadostcc, aes(x=ano, na.rm = TRUE)) +
                geom_line(aes(y=bubalino)) +
                xlab("Ano") +
                ylab("Poulação de Bufalos") +
                scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
                theme_bw()

anogalinaceos <- ggplot(dadostcc, aes(x=ano, na.rm = TRUE)) +
                 geom_line(aes(y=galinaceos)) +
                 xlab("Ano") +
                 ylab("Poulação de Galinhas") +
                 scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
                 theme_bw()

#juntando os plots em uma imagem so

ggarrange(anocerrado, anoagro, anoqueimada, anobovino, anobubalinos, anosuinos, anogalinaceos + #seleciona os objetos para a construcao da imagem
          rremove("x.text"), #apaga o texto do eixo
          labels = c(), ncol = 2, nrow = 4) #indica os nomes dos graficos e o numero de graficos que tera na imagem bem como a sua disposicao

#salvando em formato JPG

ggsave("Plot1.jpeg", width = 30, height = 20, units = "cm")

#plotando os histogramas para cada uma das variaveis

options(scipen = 999) #formata os numeros retirando a notacao cientifica

histcoberveg <- ggplot(dadostcc, aes(hectares_cerrado)) + #seleciona o df e a variavel
                geom_histogram(aes(y=..density..),colour="black",fill="white",bins=10) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
                labs(x="Área Cerrado (ha)", y="Frequência") + #renomeia os eixos
                stat_function(fun = dnorm, args = list(mean=mean(dadostcc$hectares_cerrado), sd = sd(dadostcc$hectares_cerrado))) + #gera a curva de densidade
                theme_bw() #escolhe o tema do grafico

histqueimadas <- ggplot(dadostcc, aes(total_focos_queimadas)) + #seleciona o df e a variavel
                 geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 20) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="focos de queimada", y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(dadostcc$total_focos_queimadas),sd = sd(dadostcc$total_focos_queimadas))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico

histpam <- ggplot(dadostcc, aes(total_ha_plantado)) + #seleciona o df e a variavel
  geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 20) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="area plantada (ha)",y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(dadostcc$total_ha_plantado),sd = sd(dadostcc$total_ha_plantado))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico

histbovino <- ggplot(dadostcc, aes(bovino)) + #seleciona o df e a variavel
  geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 10) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="cabeças de gado", y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(dadostcc$bovino),sd = sd(dadostcc$bovino))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico

histbubalino <- ggplot(dadostcc, aes(bubalino)) + #seleciona o df e a variavel
  geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 9) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="cabeças de bufalo", y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(dadostcc$bubalino),sd = sd(dadostcc$bubalino))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico

histsuino2013 <- dadostcc[16:23,]
histsuino <- ggplot(histsuino2013, aes(suino)) + #seleciona o df e a variavel
  geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 6) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="cabeças de porco", y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(histsuino2013$suino),sd = sd(histsuino2013$suino))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico


histgalinaceos <- ggplot(dadostcc, aes(galinaceos)) + #seleciona o df e a variavel
  geom_histogram(aes(y=..density..), colour="black",fill="white",bins = 9) + #indica a construção da linha de densidade, seleciona as cores e amplitude (bins) dos dados
  labs(x="cabeças de galinha", y="Frequência") + #renomeia os eixos
  stat_function(fun = dnorm, args = list(mean=mean(dadostcc$galinaceos),sd = sd(dadostcc$galinaceos))) + #gera a curva de densidade
  theme_bw() #escolhe o tema do grafico

#juntando os plots em uma imagem so

ggarrange(histcoberveg, histqueimadas, histpam, histbovino, histbubalino, histsuino, histgalinaceos + #seleciona os objetos para a construcao da imagem
          rremove("x.text"), #apaga o texto do eixo
          labels = c("A", "B", "C","D", "E", "F", "G"), ncol = 2, nrow = 4) #indica os nomes dos graficos e o numero de graficos que tera na imagem bem como a sua disposicao

#plotando os histogramas para cada uma das variaveis

ggsave("Plot2.jpeg", width = 30, height = 20, units = "cm")

#Verificando se os dados possuem distribuicao normal

shapiro.test(dadostcc$hectares_cerrado) #tem distribuição normal p>0,05

shapiro.test(dadostcc$total_focos_queimadas) #não tem distribuição normal p<0,05

shapiro.test(dadostcc$total_ha_plantado) #tem distribuição normal p>0,05

shapiro.test(dadostcc$bovino) #não tem distribuição normal p<0,05

shapiro.test(dadostcc$bubalino) #tem distribuição normal p>0,05

shapiro.test(dadostcc$suino) #tem distribuição normal p>0,05

shapiro.test(dadostcc$galinaceos) #tem distribuição normal p>0,05

#### Fazendo a regressao dos dados ####            

#Observando o tipo de relação entre as variaveis

relinano <- ggplot(dadostcc, aes(ano, hectares_cerrado)) + #seleciona o df e a variavel
  geom_line() + #indica o tipo de grafico
  geom_point() + #indica o tipo de grafico
  labs(x="Ano", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relinqueimada <- ggplot(dadostcc, aes(total_focos_queimadas, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="Focos de queimada", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relinpam <- ggplot(dadostcc, aes(total_ha_plantado, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="Hectares plantados", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relinbov <- ggplot(dadostcc, aes(bovino, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="Cabeças de gado", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relinbub <- ggplot(dadostcc, aes(bubalino, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="Cabeças de bufalo", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relinsuino <- ggplot(dadostcc, aes(suino, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="Cabeças de porco", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

relingali <- ggplot(dadostcc, aes(galinaceos, hectares_cerrado)) + #seleciona o df e a variavel
  geom_point() + #indica o tipo de grafico
  labs(x="População de Aves", y="Cobertura Vegetal (ha)") + #renomeia os eixos
  theme_bw() #escolhe o tema do grafico

#juntando os plots

ggarrange(relinqueimada, relinpam, relinbov, relinbub, relinsuino, relingali + #seleciona os objetos para a construcao da imagem
            rremove("x.text"), #apaga o texto do eixo
            labels = c("A", "B", "C","D", "E", "F"), ncol = 2, nrow = 3) #indica os nomes dos graficos e o numero de graficos que tera na imagem bem como a sua disposicao

#salvando em imagem a relação linear entre a variavel de cobertura vegeal em funcao das demais

ggsave("Plot3.jpeg", width = 30, height = 20, units = "cm")


#Testes de correlacao

attach(dadostcc)

cor(total_focos_queimadas, hectares_cerrado) #negativa e quase inexistente perto de 0

cor(total_ha_plantado, hectares_cerrado) #negativa e forte perto de 1

cor(bovino, hectares_cerrado) #negativa e forte perto de 1

cor(bubalino, hectares_cerrado) #negativa e forte perto de 1

cor(suino, hectares_cerrado, use="complete.obs") #negativa e fraca perto de 0, e como tem NA, não vou avaliar. 

cor(galinaceos, hectares_cerrado) #negativa e forte perto de 1

cor(ano, hectares_cerrado) #negativa e forte perto de 1

pairs(dadostcc) #plota todas as correlacoes possiveis

#salvando em imagem todas as correlacoes possiveis no estudo

ggsave("Plot4.jpeg", width = 30, height = 20, units = "cm")

#Regressão linear dos dados

mod1 <- lm(hectares_cerrado ~ total_focos_queimadas + total_ha_plantado + 
           bovino + bubalino + galinaceos + suino, data=dadostcc) #gerando o modelo linear dos dados

summary(mod1) #mostrando os resultados 

mod2 <- step(mod1) #buscando um modelo mais explicativo (AIC)

summary(mod2) #mostrando os resultados do AIC, mostrando qual o melhor modelo. 

#plotando a regressão dos dados

regqueimada <- visreg(mod2, "total_focos_queimadas", gg=TRUE)
regqueimada <- regqueimada + xlab("Focos de Queimada") +
  ylab("Hectares Cerrado") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw()

regagro <-visreg(mod2, "total_ha_plantado", gg=TRUE)
regagro <-regagro + xlab("Área Plantada") +
  ylab("Hectares Cerrado") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw()

regboi <- visreg(mod2, "bovino", gg=TRUE)
regboi <- regboi + xlab("População de bovinos") +
  ylab("Hectares Cerrado") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw()

regbuba <- visreg(mod2, "bubalino", gg=TRUE)
regbuba <- regbuba + xlab("População de bufalos") +
  ylab("Hectares Cerrado") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw()

reggalinha <- visreg(mod2, "galinaceos", gg=TRUE)
reggalinha <- regbuba + xlab("População de galinhas") +
  ylab("Hectares Cerrado") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  theme_bw()

ggarrange(regqueimada, regagro, regboi, regbuba, reggalinha, #seleciona os objetos para a construcao da imagem
          rremove("x.text") , #apaga o texto do eixo
          labels = c("A", "B", "C","D", "E"), ncol = 2, nrow = 3)

#salvando em imagem todas as correlacoes possiveis no estudo

ggsave("Plot5.jpeg", width = 30, height = 20, units = "cm")

#Plotando os residuos do modelo
par(mfrow = c(1,2)) #arrumando a área de plotagem do teste dos resíduos
hist(resid(mod2), xlab = "Resíduos", ylab= "Frequência", main = "") #fazendo histgrama dos residuos
qqnorm(resid(mod2), main = "Resíduos do Modelo", xlab = "Quantis Teóricos", ylab ="Quantis da Amostra")
qqline(resid(mod2)) #mostrando os erros ajustados a reta 
shapiro.test(resid(mod2)) #testando a normalidade dos resíduos

#Construindo tabela com os resuldados da regressão

mod2%>%
  tbl_regression(
  pvalue_fun = ~style_pvalue(.x, digits = 2),) %>%
  bold_p(t = 0.05) %>%
  bold_labels() %>%
  italicize_levels() #visualizando os resultados da regressão
  export_summs(mod2)

#outras formas de plotagem dos dados
summ(mod2)
plot_summs(mod2)
avPlots(mod2)

######## FIM DO TCC ######