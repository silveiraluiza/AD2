#Script para Laboratório 1 (parte 3)

#importar tabela de dados
alunosUFCG = read.csv("C:\\Users\\Luiza\\Documents\\UFCG\\20162\\AD2\\alunosUFCGAnon.csv", header=T)

#importar libraries
library(gmodels)
library(dplyr)

alunosCC = subset(alunosUFCG, Cod_Curso == 14102100)
alunosCC = subset(alunosCC, select = c("Matricula", "Media_Disciplina", "Nome_Disciplina"))

#media por disciplina CC

media_por_disciplina = aggregate(alunosCC[, 10:10], list(alunosCC$Nome_Disciplina), mean,na.rm=TRUE)

#mediana por disciplina CC

mediana_por_disciplina = aggregate(alunosCC[, 10:10], list(alunosCC$Nome_Disciplina), median,na.rm=TRUE)

Empsoft = subset(alunosCC, Cod_Disciplina == 1411281)
SI1 = subset(alunosCC, Cod_Disciplina == 1411178)

# Agrupa-se os dados e escolhe-se apenas a última nota como representante do
# desempenho desse aluno
Empsoft = Empsoft %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)
SI1 = SI1 %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)

# Retiramos colunas desnecessárias
Empsoft = subset(Empsoft, select = c("Matricula", "Media_Disciplina"))
SI1 = subset(SI1, select = c("Matricula", "Media_Disciplina"))

# Fazemos o Merge entre os data-frames
empsoft_si1 = merge(Empsoft, SI1, by.x = "Matricula", by.y = "Matricula", na.rm = T)

x <- empsoft_si1[2:2]
y <- empsoft_si1[3:3]

cor(x,y)
 
media_por_disciplina = media_por_disciplina[order(media_por_disciplina$x, decreasing=c(TRUE)), ]
media_redux <- subset(media_por_disciplina, x > 9.0)

x <- media_redux[1]
y <- media_redux[2]

Disciplinas <- c("LSO", "OAC II", "DIDÁTICA II","DIDÁTICA ","LSI", "IPAL","Cálculo I","Cálculo II","Vetorial","Probabilidade")
Notas <- c(10, 9.76, 9.64, 9.42, 9.15, 3.4, 3.6, 3.7, 4.4, 4.7)
df =data.frame(Disciplinas, Notas)   

plot(df)
