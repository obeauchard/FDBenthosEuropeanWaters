sclass.lab <- function(sco, fac, wt = rep(1, length(fac)), xax = 1, yax = 2, 
                       labels = levels(fac), cex.lab = 1, dia = NULL,
                       fg = rep(1, nlevels(fac)), bg = rep("white", nlevels(fac)),
                       lwd = 1, col.lab = rep(1, nlevels(fac)), font = 1){
  sco <- as.matrix(sco[c(xax, yax)])
  w <- model.matrix(~ fac - 1)
  w <- diag(wt) %*% w
  w <- sweep(w, 2, apply(w, 2, sum), "/")
  sco <- data.frame(t(w) %*% sco)
  w <- par("usr")[2] - par("usr")[1]
  if(is.null(dia))
    dia <- 0.04 * w
  par(mar = rep(0.1, 4))
  if(dia == 0){
    for(i in 1:length(labels)){
      text(sco[,1][i], sco[,2][i], labels[i], cex = cex.lab, col = col.lab[i], font = font)
    }
  }else{
    for(i in 1:length(labels)){
      symbols(sco[,1][i], sco[,2][i], circles = dia/2, inches = F,
              lwd = lwd, fg = fg[i], bg = bg[i], add = T)
      text(sco[,1][i], sco[,2][i], labels[i], cex = cex.lab, col = col.lab[i], font = font)
    }
  }
}
