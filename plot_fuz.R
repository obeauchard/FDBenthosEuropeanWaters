plot.fuz <- function(sco, tab, xax, yax, indic, which.traits = 1:max(indic),
                     plot.new = T, nr = 3, nc = 3, xlim = NULL, ylim = NULL,
                     sub = paste(rep("Block", length(tr)), tr, sep = " "),
                     possub = "topleft", csub = 2, lab.mod = colnames(tab), clab = 1.5,
                     cstar = 0, cell = 1.5, pch = 1, col = 8,
                     lwd = 0.7, lwd.p = 0.7, cex.p = 1.2, cgrid = 1.5,
                     sco.cr = NULL, cex.cr = 1, side.cr = c(2, 3),
                     adj.cr.x = c(0.12, 0.06), adj.cr.y = c(0.06, 0.12)){
  tab <- tab[indic %in% which.traits]
  lab.mod <- lab.mod[indic %in% which.traits]
  indic <- indic[indic %in% which.traits]
  indic <- rep(1:length(unique(indic)), table(indic))
  sco.cr <- sco.cr[which.traits,]
  if(plot.new == T)
    par(mfrow = c(nr, nc))
  par(mar = rep(0.1, 4))
  for(i in 1:max(indic)){
    par(lwd = lwd)
    if(is.null(xlim)){
      xlim <- range(sco[xax])
      xlim[1] <- xlim[1] + 0.15 * diff(xlim)
      xlim[2] <- xlim[2] - 0.15 * diff(xlim)
    }
    if(is.null(xlim)){
      ylim <- range(sco[,yax])
      ylim[1] <- ylim[1] + 0.15 * diff(ylim)
      ylim[2] <- ylim[2] - 0.15 * diff(ylim)
    }
    s.label(sco[c(xax, yax)], clab = 0,
            xlim = xlim, ylim = ylim, cpoint = 0, cgrid = cgrid)
    par(lwd = lwd.p)
    points(sco[c(xax, yax)], pch = pch, cex = cex.p, col = col)
    par(lwd = lwd)
    s.distri(sco[c(xax, yax)], tab[indic == i], lab = lab.mod[indic == i],
             clab = clab, sub = sub[i], possub = "topleft", csub = csub, cstar = cstar,
             cell = cell, cpoint = 0, add.p = T)
    if(is.null(sco.cr) == F){
      cr <- as.character(sco.cr[c(xax, yax)][i,])
      u <- par("usr")
      text(u[side.cr[1]] - adj.cr.x[1] * u[side.cr[1]], adj.cr.y[1] * u[side.cr[1]],
           cr[1], font = 3, cex = cex.cr)
      text(adj.cr.x[2] * u[side.cr[2]], u[side.cr[2]] - adj.cr.y[2] * u[side.cr[2]],
           cr[2], srt = 90, font = 3, cex = cex.cr)
    }
  }
}
