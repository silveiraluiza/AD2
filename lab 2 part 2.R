# Importando dados

library(readr)
library(dplyr)

graduados <- read_csv("graduados.csv", col_types = cols(matricula = col_character())) %>%
  mutate(matricula = as.factor(matricula))
head(graduados)

str(graduados)
summary(graduados)
View(graduados)
graduados <- graduados %>%
  arrange(matricula)

# Limpando dados

graduados.clean <- graduados %>% filter(!is.na(media))

summary(graduados.clean)
View(graduados.clean)

#Agora, vamos calcular o CRA dos alunos:

graduados.cra <- graduados.clean %>%
  group_by(matricula) %>%
  mutate(cra.contrib = media*creditos) %>%
  summarise(cra = sum(cra.contrib)/sum(creditos))

head(graduados.cra)

# Ajustar formato dos dados

library(reshape2)

graduados.model.input <- graduados.clean %>%
  group_by(matricula,disciplina)  %>%
  filter(media == max(media)) %>%
  ungroup() %>%
  select(matricula,disciplina,media) %>%
  mutate(disciplina = as.factor(gsub(" ",".",disciplina))) %>%
  dcast(matricula ~ disciplina, mean) %>%
  merge(graduados.cra)

head(graduados.model.input)
View(graduados.model.input)

# Criar tabelas com disciplinas do primeiro período

Disciplinas <- c("matricula", "Álgebra.Vetorial.e.Geometria.Analítica","Cálculo.Diferencial.e.Integral.I","Introdução.à.Computação","Laboratório.de.Programação.I","Leitura.e.Produção.de.Textos","Programação.I", "cra")
graduados.primeiro.per <- graduados.model.input[Disciplinas]

# Criar tabelas com disciplinas do segundo período
Disciplinas1 <- c("matricula", "Cálculo.Diferencial.e.Integral.II","Matemática.Discreta","Programação.II","Teoria.dos.Grafos","Fundamentos.de.Física.Clássica","Laboratório.de.Programação.II", "cra")
graduados.segundo.per <- graduados.model.input[Disciplinas1]

# Criar tabelas com disciplinas dos dois períodos
Disciplinas2<- c("matricula", "Cálculo.Diferencial.e.Integral.II","Matemática.Discreta","Programação.II","Teoria.dos.Grafos","Fundamentos.de.Física.Clássica","Laboratório.de.Programação.II", "Álgebra.Vetorial.e.Geometria.Analítica","Cálculo.Diferencial.e.Integral.I","Introdução.à.Computação","Laboratório.de.Programação.I","Leitura.e.Produção.de.Textos","Programação.I", "cra")
graduados.ambos.per <- graduados.model.input[Disciplinas2]

#limpando as tabelas


graduados.primeiro.per <- na.omit(graduados.primeiro.per)
graduados.segundo.per <- na.omit(graduados.segundo.per)
graduados.ambos.per <- na.omit(graduados.ambos.per)

### Removendo matricula
require(dplyr)
graduados.primeiro.per  <- graduados.primeiro.per  %>% select(-matricula)
graduados.segundo.per  <- graduados.segundo.per  %>% select(-matricula)
graduados.ambos.per  <- graduados.ambos.per  %>% select(-matricula)

# Examinando Histograma das variáveis

require(reshape2)
require(ggplot2)

df <- melt(graduados.primeiro.per)
ggplot(df,aes(x = value)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_histogram()

df1 <- melt(graduados.segundo.per)
ggplot(df1,aes(x = value)) +
  facet_wrap(~variable, scales = "free_x") +
  geom_histogram()


# Correlação

correlationMatrix <- cor(graduados.primeiro.per)
print(correlationMatrix)

# Analisando graficamente a matriz de correlação

library(corrplot)
corrplot(correlationMatrix, method="number", type="lower", order="hclust")

# Correlação do primeiro período

lm1 <- lm(cra ~ Álgebra.Vetorial.e.Geometria.Analítica + Cálculo.Diferencial.e.Integral.I + Introdução.à.Computação + Laboratório.de.Programação.I + Leitura.e.Produção.de.Textos + Programação.I, data= graduados.primeiro.per)

lm1

summary(lm1)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm1)


# Correlação do segundo período

correlationMatrix2 <- cor(graduados.segundo.per)
print(correlationMatrix2)

# Analisando graficamente a matriz de correlação 2

library(corrplot)
corrplot(correlationMatrix2, method="number", type="lower", order="hclust")

lm2 <- lm(cra ~ Cálculo.Diferencial.e.Integral.II + Matemática.Discreta + Programação.II + Teoria.dos.Grafos + Fundamentos.de.Física.Clássica + Laboratório.de.Programação.II, data= graduados.segundo.per)

lm2

summary(lm2)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm2)

# Correlação de ambos períodos

correlationMatrix3 <- cor(graduados.ambos.per)
print(correlationMatrix3)

# Analisando graficamente a matriz de correlação 3

library(corrplot)
corrplot(correlationMatrix3, method="number", type="lower", order="hclust")

lm3 <- lm(cra ~ Álgebra.Vetorial.e.Geometria.Analítica +
            Cálculo.Diferencial.e.Integral.I + Introdução.à.Computação
          + Laboratório.de.Programação.I + Leitura.e.Produção.de.Textos + 
            Programação.I + Cálculo.Diferencial.e.Integral.II + Matemática.Discreta
          + Programação.II + Teoria.dos.Grafos + Fundamentos.de.Física.Clássica
          + Laboratório.de.Programação.II, data= graduados.ambos.per)

lm3

summary(lm3)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm3)


# Um modelo de regressão múltipla com todas as variáveis é plausível para explicar a variação em y? Em que grau?

# Observando o modelo com todas as variáveis vemos que é plausível, 
# porém quando separamos por período existem mais variáveis que podem ser destacadas.

# Todas as variáveis são úteis para o modelo de regressão?

# Até certo ponto, podemos observar variáveis mais importantes que outras, portanto
# podemos construir um modelo melhor usando apenas elas.

# Se a resposta para a pergunta anterior foi não, construa um novo modelo sem essas variáveis e o compare ao modelo com todas as variáveis (e.g. em termos de R2 e RSE).

#Modelo para o primeiro período

require(dplyr)

graduados.primeiro.per.novo  <- graduados.primeiro.per  %>% 
  select(-Programação.I,-Laboratório.de.Programação.I,-Leitura.e.Produção.de.Textos)
correlationMatrix4 <- cor(graduados.primeiro.per.novo)
print(correlationMatrix4)

library(corrplot)
corrplot(correlationMatrix4,  method="color",   
         type="lower", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE )

lm1.novo <- lm(cra ~ Álgebra.Vetorial.e.Geometria.Analítica + Cálculo.Diferencial.e.Integral.I + Introdução.à.Computação, data= graduados.primeiro.per)

lm1.novo

summary(lm1.novo)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm1.novo)

#Modelo para o segundo período

require(dplyr)

graduados.segundo.per.novo  <- graduados.segundo.per  %>% 
  select(-Cálculo.Diferencial.e.Integral.II,-Laboratório.de.Programação.II,-Fundamentos.de.Física.Clássica)
correlationMatrix5 <- cor(graduados.segundo.per.novo)
print(correlationMatrix5)

library(corrplot)

corrplot(correlationMatrix5, method="color",   
         type="lower", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE 
)
library("GGally")
ggcorr(correlationMatrix5, palette = "RdBu", label = TRUE)
ggpairs(graduados.primeiro.per.novo)

lm2.novo <- lm(cra ~ Matemática.Discreta + Programação.II + Teoria.dos.Grafos, data= graduados.segundo.per)

lm2.novo

summary(lm2.novo)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(lm2.novo)


library(caret)
predicoes = predict.lm(lm1,graduados.primeiro.per)
cra <- graduados.primeiro.per$cra
residuos = cra - predicoes
axisRange = extendrange(c(cra,predicoes)) #deixando as variáveis na mesma escala
plot(cra,predicoes)
abline(0,1,col="blue",lty=2,lwd=2)

predicoes = predict.lm(lm2,graduados.segundo.per)
cra <- graduados.segundo.per$cra
residuos = cra - predicoes
axisRange = extendrange(c(cra,predicoes)) #deixando as variáveis na mesma escala
plot(cra,predicoes)
abline(0,1,col="blue",lty=2,lwd=2)

predicoes = predict.lm(lm3,graduados.ambos.per)
cra <- graduados.ambos.per$cra
residuos = cra - predicoes
axisRange = extendrange(c(cra,predicoes)) #deixando as variáveis na mesma escala
plot(cra,predicoes)
abline(0,1,col="blue",lty=2,lwd=2)

predicoes = predict.lm(lm1.novo,graduados.primeiro.per.novo)
cra <- graduados.primeiro.per.novo$cra
residuos = cra - predicoes
axisRange = extendrange(c(cra,predicoes)) #deixando as variáveis na mesma escala
plot(cra,predicoes)
abline(0,1,col="blue",lty=2,lwd=2)

predicoes = predict.lm(lm2.novo,graduados.segundo.per.novo)
cra <- graduados.segundo.per.novo$cra
residuos = cra - predicoes
axisRange = extendrange(c(cra,predicoes)) #deixando as variáveis na mesma escala
plot(cra,predicoes)
abline(0,1,col="blue",lty=2,lwd=2)

plot(predicoes,residuos)
abline(h=0,col="blue",lty=2,lwd=2)

qqnorm(residuos)
qqline(residuos, col = 2,lwd=2,lty=2)

# Analise os plots de resíduos de cada variável e veja se algum (um ou mais) deles indica não aleatoriedade dos erros.
# Que período consegue explicar melhor o desempenho final (primeiro ou segundo)?
# Use o melhor modelo encontrado para predizer o seu próprio desempenho e compare a predição com o seu CRA atual. Comente o resultado.
