cor.ratio <- function(sco, fuz, indic, lab = NULL){
  df <- data.frame(row.names = 1:max(indic))
  for(i in 1:ncol(sco)){
    sco.mat <- matrix(rep(sco[,i], nr = nrow(fuz), nc = ncol(fuz)))
    px <- fuz * sco.mat
    mean.x <- apply(px, 2, sum) / apply(fuz, 2, sum)
    mean.p <- apply(fuz, 2, mean)
    cr <- mean.p * mean.x^2
    cr <- tapply(cr, factor(indic), sum)
    cr <- as.character(round(cr, 2))
    cr[cr == "0"] <- "0.00"
    cr[nchar(cr) == 3] <- paste(cr[nchar(cr) == 3], "0", sep = "")
    cr <- data.frame(cr)
    colnames(cr) <- paste("Axis", i, sep = " ")
    df <- cbind(df, cr)
    }
  if(is.null(lab) == F) rownames(df) <- lab
  return(df)
}
