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
  
  fitSumm[is.na(fitSumm)] <- ""
  
  return(list(fit, fitSumm))
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