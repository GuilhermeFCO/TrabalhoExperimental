library(magrittr)
library(ggplot2)

descritivaContinua <- function(df, variaveis, grupo = NULL) {
  
  for (i in 1:length(variaveis)) {
    if (is.null(grupo)) {
      aux <- df %>%
        dplyr::summarise("Mediana" = round(median(!!sym(variaveis[i])), 2),
                         "Média" = round(mean(!!sym(variaveis[i])), 2),
                         "Mínimo" = round(min(!!sym(variaveis[i])), 2),
                         "Máximo" = round(max(!!sym(variaveis[i])), 2),
                         "Desvio Padrão" = round(sd(!!sym(variaveis[i])), 2),
                         "Variância" = round(var(!!sym(variaveis[i])), 2),
                         "N" = dplyr::n())
      aux[, "Variável"] <- variaveis[i]
      aux <- aux %>% dplyr::relocate("Variável", .before = 1)
    } else if (length(grupo) == 1) {
      aux <- df %>%
        dplyr::group_by(!!sym(grupo)) %>%
        dplyr::summarise("Mediana" = round(median(!!sym(variaveis[i])), 2),
                         "Média" = round(mean(!!sym(variaveis[i])), 2),
                         "Mínimo" = round(min(!!sym(variaveis[i])), 2),
                         "Máximo" = round(max(!!sym(variaveis[i])), 2),
                         "Desvio Padrão" = round(sd(!!sym(variaveis[i])), 2),
                         "Variância" = round(var(!!sym(variaveis[i])), 2),
                         "N" = dplyr::n())
      aux[, "Variável"] <- variaveis[i]
      aux <- aux %>% dplyr::relocate("Variável", .before = 1)
    } else {
      
      aux <- df %>%
        dplyr::group_by(!!!syms(grupo)) %>%
        dplyr::summarise("Mediana" = round(median(!!sym(variaveis[i])), 2),
                         "Média" = round(mean(!!sym(variaveis[i])), 2),
                         "Mínimo" = round(min(!!sym(variaveis[i])), 2),
                         "Máximo" = round(max(!!sym(variaveis[i])), 2),
                         "Desvio Padrão" = round(sd(!!sym(variaveis[i])), 2),
                         "Variância" = round(var(!!sym(variaveis[i])), 2),
                         "N" = dplyr::n())
      aux[, "Variável"] <- variaveis[i]
      aux <- aux %>% dplyr::relocate("Variável", .before = 1)
    }
    
    if (i == 1) resultado <- aux
    else resultado <- resultado %>% rbind(aux)
  }
  
  return(resultado %>% data.frame())
}

descritivaCategorica <- function(df, variaveis, grupo) {
  
  for (i in 1:length(variaveis)) {
    aux <- df %>%
      dplyr::group_by(!!!syms(c(variaveis[i], grupo))) %>% 
      dplyr::summarise("N" = dplyr::n())
    aux[, "Variável"] <- variaveis[i]
    aux <- aux %>% dplyr::relocate("Variável", .before = 1)
    
    colnames(aux)[2] <- "Grupo"
    
    if (i == 1) resultado <- aux
    else resultado <- resultado %>% rbind(aux)
  }
  
  return(resultado %>% data.frame())
}

Anova <- function(df, Formula, label) {
  fit <- aov(formula(Formula), data = df)
  fitSumm <- summary(fit)[[1]]
  fitSumm[,"Variável"] <- row.names(fitSumm)
  fitSumm <- fitSumm %>% dplyr::relocate("Variável", .before = 1)
  row.names(fitSumm) <- NULL
  colnames(fitSumm) <- c(
    "Variável",
    "Graus de Liberdade",
    "Soma dos Quadrados",
    "Média SQ",
    "Valor F",
    "P Valor"
  )
  
  fitSumm[,3:6] <- round(fitSumm[,3:6], 3)
  
  for (i in 1:length(label)) fitSumm[i, 1] <- label[i]
  
  fitSumm[nrow(fitSumm), 1] <- "Resíduos"
  
  fitSumm[is.na(fitSumm)] <- " - "
  
  return(list(fit, fitSumm))
}

pressupostos <- function(fit, tratamento = NULL, bloco = NULL, df) {
  car::qqPlot(fit$residuals, pch=19, col.lines="darkred", id=F)
  print(shapiro.test(fit$residuals))
  if (!is.null(tratamento)) {
    plot(as.numeric(tratamento),fit$residuals,pch=19, 
         main = "Resíduo x Tratamentos", xlab='tratamento', ylab='resíduos')
    abline(h=0)
    
    print(bartlett.test(fit$residuals ~ tratamento, data=df))
  }
  if (!is.null(bloco)) {
    plot(as.numeric(bloco),fit$residuals,pch=19, 
         main='Resíduo x Blocos', xlab='bloco', ylab='resíduos')
    abline(h=0)
    
    print(artlett.test(fit$residuals ~ bloco, data=df))
  }
}


boxplot <- function(df, Y, X, grupo = NULL, Title = NULL, labelY = Y, labelX = X, legend = grupo) {
  if (is.null(Title)) Title <- ""
  
  if (is.null(grupo)) {
    
    grafico <- ggplot(df, aes(y = !!sym(Y), x = !!sym(X))) +
      geom_boxplot(fill = c("#3c61b0")) +
      theme_bw() +
      labs(
        title = Title,
        y = labelY,
        x = labelX
      ) +
      theme(
        plot.title = element_text(color = "#000000", size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      )
    
  } else {
    
    grafico <- ggplot(df, aes(y = !!sym(Y), x = !!sym(X), fill = !!sym(grupo))) +
      geom_boxplot() +
      theme_bw() +
      labs(
        title = Title,
        y = labelY,
        x = labelX,
        fill = legend
      ) +
      theme(
        plot.title = element_text(color = "#000000", size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold")
      ) +
      scale_fill_manual(values = c("#3c61b0", "#95b2f0"))
    
  }
  
  return(grafico)
}