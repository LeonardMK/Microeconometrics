# Function to get results into the same format as in pandas
results <- function(models, coeff = NULL, r = 2, r_R2 = 2, R2 = TRUE, TE = NULL, FE = NULL, Obs = TRUE, labels = NULL){
  
  # If coeff = NULL use all coefficients
  if(is.null(coeff)){
  
    coeff = names(models[[length(models)]]$coefficients)
  
  }
  # Create empty dataframe
  M <- matrix(nrow = length(coeff) * 2, ncol = length(models))
  
  # Adjust rownames
  row.names(M) <- paste0(rep(coeff, each = 2), c("", "_SE"))
  
  # Including coefficients
  for(i in seq_along(models)){
    
    # Insert columnwise
    m_coeff <- coeff[coeff %in% names(models[[i]]$coefficients)]
    M[m_coeff, i] <- paste0(round(models[[i]]$coefficients[m_coeff], r), " ", cut(summary(models[[i]])$coefficients[m_coeff, 4], c(0, 0.01, 0.05, 0.1, 1), c("***", "**", "*", "")))
    M[paste0(m_coeff, "_SE"), i] <- paste0("(", round(summary(models[[i]])$coefficients[m_coeff, 2], r), ")")
    
  }
  
  # Replace NA values
  M[is.na(M)] <- ""
  
  # Turn into datafram
  M <- data.frame(M, stringsAsFactors = FALSE)
  colnames(M) <- 1:length(models)
  
  # Replace labels
  if(!is.null(labels)){
    
    row.names(M) <- paste0(rep(labels, each = 2), c("", " SE"))
    
    }
  
  # R2
  if(R2){
    
    # Null model
    L0 = update(models[[1]], . ~ 1)$loglik[2]
    L1 = sapply(1:length(models), function(i) models[[i]]$loglik[2])
    
    pseudoR = round(1 - L1 / L0, r_R2)
    M <- rbind.data.frame(M, `Mc-Fadden R2` = as.character(pseudoR))
    
  }
  
  # TE
  if(!is.null(TE)){
    
    M <- rbind.data.frame(M, `Time controls` = TE)
    
  }
  
  # FE
  if(!is.null(FE)){
    
    M <- rbind.data.frame(M, `Fixed effects` = FE)
    
  }
  
  # Obs
  # TE
  if(Obs){
    
    M <- rbind.data.frame(M, Observations = sapply(seq_along(models), function(i) length(models[[i]]$linear.predictors)))
    
  }
  
  # Return final matrix
  return(M)
  
}
