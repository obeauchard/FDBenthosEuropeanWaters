eb <- function(x, y, m = mean, bar = "ci", l = 0.25, h = 0.75,
               pch.m = 21, pch.p = 1, cex.m = 1.5, cex.p = 1,
               bg = 1, col.m = 1, col.p = "grey60", col.b = 1,
               lwd = 1, len.seg = 0, xaxt = "l", yaxt = "l", las = 1,
               ylim = range(pretty(y)), ax.grp = F, aty = pretty(y), mod = NULL, 
               xlab = "", ylab = "", xlab.side = 1, ylab.side = 2,
               cex.mod = 1, laby = T, cex.axis = 1, cex.lab = 1.5,
               xlab.or = 1, ylab.or = 1, line.val = 1, line.ax = NA,
               line.mod = 1, line.lab = c(3, 3),
               title = "", font.main = 1, cex.title = 1, line.title = 4, adj = 0,
               sort = F, dec = F, horiz = F, depth = F, horiz.ax = 1,
               lines = F, jt = T, am = 0.15, bty = "n", return.x = F){
  if(is.data.frame(x) == F){
    df <- F
    lev <- levels(x)
    x <- as.numeric(x)
    y <- y[order(x)]
    x <- sort(x)
  }else{
    df <- T
    dfx <- x
    lev <- levels(x[,1])
    for(i in 1:ncol(x)){
      x[,i] <- as.numeric(x[,i])
    }
    y <- y[do.call(order, x)]
    x <- x[do.call(order, x),]
    f <- rep(1, nrow(x))
    for(i in 1:ncol(x)){
      w <- c(0, diff(x[,i]))
      w[w != 0] <- 1
      w <- cumsum(w)
      f <- f + w
    }
    x <- f
    rep.col <- length(unique(x))
  }
  if(ax.grp == T){
    ngrp <- length(unique(x)) / length(unique(dfx[,ncol(dfx)]))
    w <- gl(ngrp, length(unique(dfx[,ncol(dfx)])), length(unique(x)))
    atx <- tapply(unique(x), w, mean)
    if(depth == T) atx <- -atx
  }
  if(sort == T){
    y <- y[order(x)]
    x <- sort(x)
    w <- tapply(y, factor(x), m)
    w <- rep(w, table(x))
    x <- x[order(w, decreasing = dec)]
    y <- y[order(w, decreasing = dec)]
    mod <- lev[order(tapply(y, factor(x), m), decreasing = dec)]
    x <- rep(1:length(mod), table(x)[order(tapply(y, factor(x), m), decreasing = dec)])
  }
  if(depth == T) x <- -x
  if(df == T){
    if(depth == F){
      col.p <- rep(rep(col.p, rep.col / length(col.p)), table(x))
    }else{
      col.p <- rep(rep(col.p, rep.col / length(col.p)), table(-x))
    }
    col.m <- rep(col.m, rep.col)
    col.b <- rep(col.b, rep.col)
    bg <- rep(bg, rep.col)
  }
  if(horiz == F){
    abs <- x
    if(jt == T){
      abs <- jitter(abs, amount = am)
    }
    ord <- y
    xlim <- c(min(abs) - 0.25, max(abs) + 0.25)
  }else{
    abs <- y
    ord <- x
    if(jt == T){
      ord <- jitter(ord, amount = am)
    }
    xlim <- c(ylim[1], ylim[2])
    ylim <- c(min(ord) - 0.25, max(ord) + 0.25)
  }
  par(mgp = c(3, line.val, 0))
  plot(abs, ord, pch = pch.p, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n",
    xlab = "", ylab = "", cex = cex.p, col = col.p, xaxs = "r", yaxs = "r", bty = "n")
  x1 <- unique(x)
  x2 <- unique(x)
  if(bar == "sd"){
    stat <- function(x){sd(x)}
  }else if(bar == "ci"){
    stat <- function(x){1.96 * sd(x) / length(x)^0.5}
  }else if(bar == "se"){
    stat <- function(x){sd(x) / length(x)^0.5}
  }else if(bar == "pc"){
    stat1 <- function(x){quantile(x, l)}
    stat2 <- function(x){quantile(x, h)}
  }else if(bar == "m.seg"){
    stat <- function(x){m(x)}
  }
  if(bar %in% c("pc", "m.seg") == F){
    y1 <- tapply(y, factor(x), m) - tapply(y, factor(x), stat)
    y2 <- tapply(y, factor(x), m) + tapply(y, factor(x), stat)
  }else{
    if(bar == "pc"){
      y1 <- tapply(y, factor(x), stat1)
      y2 <- tapply(y, factor(x), stat2)
    }else{
      y1 <- tapply(y, factor(x), stat)
      y2 <- tapply(y, factor(x), stat)
    }
  }
  y1[is.na(y1)] <- 0
  y2[is.na(y2)] <- 0
  if(horiz == F){
    abs <- unique(x)
    ord <- tapply(y, factor(x), m)
  }else{
    abs <- tapply(y, factor(x), m)
    ord <- unique(x)
    if(depth == T){
      abs <- rev(abs)
      y1 <- rev(y1)
      y2 <- rev(y2)
    }
  }
  ord[is.na(ord)] <- 0
  if(bar != "m.seg"){
    if(horiz == F){
      segments(x1, y1, x2, y2, col = col.b, lwd = lwd)
      segments(x1 - len.seg, y1, x2 + len.seg, y1, col = col.b, lwd = lwd)
      segments(x1 - len.seg, y2, x2 + len.seg, y2, col = col.b, lwd = lwd)
    }else{
      segments(y1, x1, y2, x2, col = col.b, lwd = lwd)
      segments(y1, x1 - len.seg, y1, x2 + len.seg, col = col.b, lwd = lwd)
      segments(y2, x1 - len.seg, y2, x2 + len.seg, col = col.b, lwd = lwd)
    }
  }
  if(bar != "m.seg"){
    points(abs, ord, pch = pch.m, cex = cex.m, col = col.m, bg = bg)
  }else{
    if(horiz == F){
      segments(x1 - len.seg, ord, x2 + len.seg, ord, col = col.b, lwd = lwd) 
    }else{
      segments(abs, x1 - len.seg, abs, x2 + len.seg, col = col.b, lwd = lwd)
    }
  }
  if(lines == T) lines(abs, ord, type = "b", pch = "")
  if(xaxt != "n"){
    if(horiz == F){
      atx <- abs
    }else{
      if(ax.grp == T){
        atx <- atx
      }else{
        atx <- ord
      }
    }
    if(is.null(mod)) mod <- lev
    axis(side = ifelse(horiz == F, 1, 2), at = atx,
         labels = F, line = line.ax[1])
    mtext(side = ifelse(horiz == F, 1, 2), at = atx, mod,
          cex = cex.mod, line = line.mod, las = xlab.or)
  }
  if(yaxt != "n"){
    par(cex.axis = cex.axis)
    axis(side = ifelse(horiz == F, 2, horiz.ax), at = aty, line = line.ax[2],
         las = ylab.or, labels = laby)
  }
  mtext(side = xlab.side, ifelse(horiz == F, xlab, ylab), cex = cex.lab, line = line.lab[1])
  mtext(side = ylab.side, ifelse(horiz == F, ylab, xlab), cex = cex.lab, line = line.lab[2])
  title(main = title, font.main = font.main, line = line.title, cex.main = cex.title, adj = adj)
  if(return.x == T) return(x)
  #x...............factor, numeric vector or data frame
  #y...............numeric vector
  #m...............central parameter, mean or median
  #bar.............either standard deviation ("sd"), standard error ("se"),
  #                confidence interval ("ci"), percentiles ("pc") or unique central bar ("m.seg")
  #l...............if bar = "pc", lower percentile
  #h...............if bar = "pc", higher percentile
  #mod.............character vector for x modalities
  #xaxt............x axis type
  #yaxt............y axis type
  #xlab............x axis name
  #ylab............y axis name
  #xlab.side.......side of x axis name
  #ylab.side.......side of y axis name
  #laby............if true, displays y axis labels
  #title...........graph title
  #cex.title.......graph title size
  #xlab.or.........x modalities orientation
  #ylab.or.........y labels orientation
  #cex.lab.........x and y axis name size
  #cex.axis........y axis label size
  #cex.mod.........modalities size	
  #pch.m...........symbol type for m values
  #pch.p...........symbol type for individual values
  #cex.m...........symbol size for mean values
  #cex.p...........symbol size for values within groups
  #bg..............colour to be used for the background
  #col.m...........colour for values for m values
  #col.p...........colour for within groups
  #col.b...........colour for bars
  #lwd.............bar width
  #len.seg.........staple length
  #las.............orientation of axis labels
  #line.ax.........distance between axis and plot
  #line.mod........distances between modalities and x/y axis
  #line.lab........distances between axis and axis name
  #line.title......distance between the graph title and the graph
  #atx.............positions of x axis modalities
  #aty.............positions of y axis labels
  #xlim............x limits
  #ylim............y limits
  #ax.grp..........if true, modality labels attributed per group
  #sort............if true, sort modalities
  #dec.............if sort is true, sorting order
  #horiz...........if true, horizontal graph
  #horiz.ax........if horiz is true, side of the y axis
  #depth...........if true, horizontal graph and negative x levels
  #lines...........if true, draw a line
  #jt..............if true, jittering of dots
  #am..............if jt is true, jittering amount
  #bty.............background type
  #return.x........if true, returns x coordinates
}
