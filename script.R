# Introdução ao Data Analytics:

# -----------------------------------------------
# AULA 01 Carregando dados e definindo o problema
# -----------------------------------------------
library(Ecdat)
data(Produc)
# Produçao dos estados dos EUA
# state=the state, year=the year, pcap=estoque de capital privado,hwy=highway and streets, water= water and sewer facilities, util=other public buildings and structures, pc=public capital,gsp=gross state products,emp=labor input measured by the employment in non–agricultural payrolls,unemp=state unemployment rate

# Pergunta
# O que afetou a taxa de desemprego ao longo dos anos?
# O aumento do capital público diminuiu a taxa de desemprego?

# -----------------------------------------------
# AULA 02 Análise exploratória dos dados
# -----------------------------------------------
summary(Produc)

library(DataExplorer)
plot_histogram(Produc)
plot_missing(Produc)

# -----------------------------------------------
# AULA 03 Matrix de correção
# -----------------------------------------------
library(DataExplorer)
plot_correlation(Produc)

# outros pacotes não trabalham com não-numerico, remover coluna de estados
Produc2<-Produc[,-1]

library(PerformanceAnalytics)
chart.Correlation(Produc2, histogram = TRUE, pch = 19)

# -----------------------------------------------
# AULA 04 Primeiros modelos
# -----------------------------------------------

m1<-lm(unemp~year, data=Produc)
anova(m1, test="F")


# -----------------------------------------------
# AULA 05 Aumentando o modelo
# -----------------------------------------------

m2<-lm(unemp~state, data=Produc)
anova(m2,test="F")

m3<-lm(unemp~state+year, data=Produc)
anova(m3,test="F")

# O problema de usar todas as variáveis
m4<-lm(unemp~.,data=Produc)
anova(m4, test="F")


# -----------------------------------------------
# AULA 06 Normalidade dos dados
# -----------------------------------------------

# criando exemplo
shapiro.test(Produc$unemp)
names(Produc)
mcompletop<-glm(unemp~year+pc+pcap+util+gsp,data=Produc, family = poisson)
anova(mcompletop, test="F")
mcompleto<-glm(unemp~util+year+pc+pcap+gsp,data=Produc)
anova(mcompleto, test="F")

# -----------------------------------------------
# AULA 07 Gráficos e conclusão
# -----------------------------------------------
library(ggplot2)
ggplot(Produc, aes(x=pc, y=unemp)) + geom_point() +
  geom_smooth(method="glm", formula=y~log(x)) +
  geom_smooth(method="lm",color="red")

ggplot(Produc, aes(x=pcap, y=unemp)) + geom_point() +
  geom_smooth(method="glm", formula=y~log(x)) +
  geom_smooth(method="lm",color="red")

ggplot(Produc, aes(x=pcap, y=unemp, fill=state)) + geom_point() +
  geom_smooth(method="glm", formula=y~log(x), aes(color=state),se=F)

ggplot(Produc, aes(x=state,y=unemp)) + geom_point() +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=1))
  
# -----------------------------------------------
# AULA 08 Importância das variáveis e Multicolinearidade
# -----------------------------------------------

library(caret)
varImp(mcompletop)
varImp(mcompleto)

# Multicolinearidade
mcompletop<-glm(unemp~year+pc+pcap+util+gsp,data=Produc, family = poisson)
mcompleto<-glm(unemp~util+year+pc+pcap+gsp,data=Produc, family = poisson)

drop1(mcompleto, test = "F")

# fazendo outro teste
mcompleto2<-glm(unemp~.,data=Produc)
drop1(mcompleto2, test = "F")

# state=the state, year=the year
#pc=public capital
#pcap=estoque de capital privado
#util=other public buildings and structures
#gsp=gross state products

# -----------------------------------------------
# AULA 09 Conclusão e gráficos
# -----------------------------------------------

mcompleto<-glm(unemp~.,data=Produc2, family = poisson)
drop1(mcompleto, test = "F")
library(caret)
varImp(mcompleto)

library(ggplot2)
ggplot(Produc, aes(x=year, y=unemp)) + geom_point() +
  geom_smooth(formula = y~log(x), method = "glm")

ggplot(Produc, aes(x=gsp, y=unemp)) + geom_point() +
  geom_smooth(formula = y~log(x), method = "glm") #gsp=gross state products

ggplot(Produc, aes(x=pc, y=unemp)) + geom_point(aes(color=state)) +
  geom_smooth(formula = y~log(x), method = "glm")

# estado
mcompleto<-glm(unemp~.,data=Produc, family = poisson)
drop1(mcompleto, test = "F")
varImp(mcompleto)

ggplot(Produc,aes(x=state, y=unemp)) + geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  stat_summary(fun.y = mean, colour = "darkred")

# -----------------------------------------------
# AULA 10 Relatórios estatísticos com Rmarkdown
# -----------------------------------------------

# Header1  {#anchor}
## Header 2  {#css_id}
### Header 3  {.css_class}
#### Header 4
##### Header 5   
*italics* and **bold**

 *unordered list     + sub-item 1     + sub-item 2

 *** linha orizontal

# https://bookdown.org/yihui/rmarkdown/

# miktex
# https://miktex.org/

# {r , message=FALSE, warning=FALSE}

# tema
# https://www.datadreaming.org/post/r-markdown-theme-gallery/
