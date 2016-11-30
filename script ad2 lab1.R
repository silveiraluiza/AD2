# Script para Laboratório 1 de ADII (parte 2)

alunosUFCG = read.csv("C:\\Users\\Luiza\\Documents\\UFCG\\20162\\AD2\\alunosUFCGAnon.csv", header=T)

library(gmodels)
library(dplyr)

media_por_disciplina = aggregate(alunosUFCG[, 10:10], list(alunosUFCG$Nome_Disciplina), mean,na.rm=TRUE)

media_por_curso = aggregate(alunosUFCG[, 10:10], list(alunosUFCG$Nome_Curso), mean,na.rm=TRUE)

mediana_por_curso = aggregate(alunosUFCG[, 10:10], list(alunosUFCG$Nome_Curso), median,na.rm=TRUE)

alunosCC <- filter(alunosUFCG, Cod_Curso == 14102100)

media_por_periodo = aggregate(alunosCC[, 10:10], list(alunosCC$Periodo), mean,na.rm=TRUE)

mediana_por_periodo = aggregate(alunosCC[, 10:10], list(alunosCC$Periodo), median,na.rm=TRUE)

alunosDisc<- filter(alunosCC, Cod_Disciplina == 1109113)
alunosCalc <- filter(alunosCC, Cod_Disciplina == 1109103)

colnames(alunosDisc)[3] <- "Cod_Discreta"
colnames(alunosDisc)[7] <- "Discreta"
colnames(alunosDisc)[10] <- "Media_Disc"

colnames(alunosCalc)[3] <- "Cod_Calculo"
colnames(alunosCalc)[7] <- "Calculo"
colnames(alunosCalc)[10] <- "Media_Calc"

alunosDiscCalc <- merge(x = alunosDisc, y = alunosCalc, by = "Matricula", all = TRUE)

colnames(alunosDiscCalc.1)[2] <- "Media_Disciplina.x"
colnames(alunosDiscCalc.1)[3] <- "Media_Disciplina.y"
alunosDiscCalc.1 <- na.omit(alunosDiscCalc)

alunosDiscCalc.1 <- alunosDiscCalc.1[,-(3:3),drop=FALSE] 

group_by(alunosDiscCalc.1, Matricula) %>% top_n(1,Media_Disciplina.x)



discreta = subset(alunosCC, Cod_Disciplina == 1109113)
calculo = subset(alunosCC, Cod_Disciplina == 1109103)

# Agrupa-se os dados e escolhe-se apenas a última nota como representante do
# desempenho desse aluno
discreta = discreta %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)
calculo = calculo %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)

# Retiramos colunas desnecessárias
discreta = subset(discreta, select = c("Matricula", "Media_Disciplina"))
calculo = subset(calculo, select = c("Matricula", "Media_Disciplina"))

# Fazemos o Merge entre os data-frames
discreta_e_calculo = merge(calculo, discreta, by.x = "Matricula", by.y = "Matricula", na.rm = T)

x <- discreta_e_calculo[2:2]
y <- discreta_e_calculo[3:3]

cor(x,y)
