baseCDC <- openxlsx::read.xlsx("./data/baseCDC.xlsx")

baseCDC$duracao <- (as.numeric(substr(chron::times(baseCDC$duracao),1,2)) * 60) + as.numeric(substr(chron::times(baseCDC$duracao),4,5)) + (as.numeric(substr(chron::times(baseCDC$duracao),7,8)) / 60)


baseCDC$horario_inicio <- NULL
baseCDC$horario_termino <- NULL
baseCDC$curso <- "CDC"


possiveis <- row.names(baseCDC[baseCDC$tratamento == "Sem música" & baseCDC$formato_ensino == "Remoto", ])
set.seed(27)
retirar <- sample(possiveis, 1)
baseCDC <- baseCDC[-as.numeric(retirar), ]
table(baseCDC$formato_ensino, baseCDC$tratamento)
rm(possiveis, retirar)

baseCDC <- baseCDC %>% dplyr::mutate_at(c(1, 3, 4, 5, 8:28), as.factor)
baseCDC$numero_acertos <- (baseCDC$numero_acertos/20)*100

##################

baseMAT <- openxlsx::read.xlsx("./data/baseMAT.xlsx")
baseMAT$curso <- "MAT"
baseMAT$horario_inicio <- NULL
baseMAT$horario_termino <- NULL
baseMAT$duracao <- (as.numeric(substr(chron::times(baseMAT$duracao),1,2)) * 60) + as.numeric(substr(chron::times(baseMAT$duracao),4,5)) + (as.numeric(substr(chron::times(baseMAT$duracao),7,8)) / 60)

baseMAT$q1 <- ifelse(baseMAT$q1 == "d)", 1, 0)
baseMAT$q2 <- ifelse(baseMAT$q2 == "d) 130º)", 1, 0)
baseMAT$q3 <- ifelse(baseMAT$q3 == "c) 66", 1, 0)
baseMAT$q4 <- ifelse(baseMAT$q4 == "d) 6", 1, 0)
baseMAT$q5 <- ifelse(baseMAT$q5 == "c) 9", 1, 0)
baseMAT$q6 <- ifelse(baseMAT$q6 == "d)", 1, 0)
baseMAT$q7 <- ifelse(baseMAT$q7 == "c)", 1, 0)
baseMAT$q8 <- ifelse(baseMAT$q8 == "a)", 1, 0)
baseMAT$q9 <- ifelse(baseMAT$q9 == "a) g é bijetora.", 1, 0)
baseMAT$q10 <- ifelse(baseMAT$q10 == "b)", 1, 0)
baseMAT$q11 <- ifelse(baseMAT$q11 == "d)", 1, 0)
baseMAT$q12 <- ifelse(baseMAT$q12 == "Reflexiva e Anti-simétrica", 1, 0)
baseMAT$q13 <- ifelse(baseMAT$q13 == "c)", 1, 0)
baseMAT$q14 <- ifelse(baseMAT$q14 == "c", 1, 0)
baseMAT$q15 <- ifelse(baseMAT$q15 == "Verdadeiro", 1, 0)
baseMAT$q16 <- ifelse(baseMAT$q16 == "e) Ângulo Ângulo (A.A.)", 1, 0)
baseMAT$q17 <- ifelse(baseMAT$q17 == "a) Baricentro", 1, 0)
baseMAT$q18 <- ifelse(baseMAT$q18 == "Falso", 1, 0)
baseMAT$q19 <- ifelse(baseMAT$q19 == "d) Triângulo", 1, 0)
baseMAT$q20 <- ifelse(baseMAT$q20 == "b) Ângulo Lado Ângulo (A.L.A.)", 1, 0)

baseQuestoes <- baseMAT |> dplyr::select(8:27)
library(magrittr)
baseQuestoes <- baseQuestoes %>% dplyr::mutate(soma = rowSums(.))
baseMAT$numero_acertos <- baseQuestoes$soma
rm(baseQuestoes)

baseMAT <- baseMAT %>% dplyr::mutate_at(c(1, 3, 4, 5, 8:28), as.factor)
baseMAT$numero_acertos <- (baseMAT$numero_acertos/20)*100

###################

base <- rbind(baseCDC, baseMAT)

