eigen.bar <- function(eig, col = c(1, 1, rep(0, length(eig) - 2)), border = 1,
                      plt = c(0.01, 0.33, 0.01, 0.33), mar = rep(0, 4)){
  opar <- par(plt = par("plt"))
  on.exit(par(opar))
  par(new = T, mar = mar, plt = plt)
  barplot(eig, xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "",
          col = col, border = border)
}
