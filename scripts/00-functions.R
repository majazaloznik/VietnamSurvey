###############################################################################
## FUNCTIONS
###############################################################################
## 0. preliminaires
## 1.1  Swapping positions in the dataset.
## 2.1 Nominal variables Print table
## 2.2 Nominal variables barplot
## 4. consolidate all table and plot funcitons for household data
## 5. consolidate all table and plot funcitons for member data - 
## 6. consolidate all table and plot funcitons for member data in HH, not respondent
## 8 My vioplot
## 9 FunNumberGens
###############################################################################

## 0. preliminaires
###############################################################################
require(RColorBrewer)
library(vioplot)

# 1.1 Swapping positions in the dataset. Used in ##############################
###############################################################################
FunSwap <- function(ds , New, After) {# only use to move back!!
  index <- 1:ncol(ds)
  from <- which(names(ds)[index] == New)
  to <- which(names(ds)[index] == After)
  index <- append(index, values = from, after = to) # insert in new position
  index <- index[-(from+1)] # remove from old
  return(ds[index])
}


## 2.1  Nominal variables Print table ########################################
## `FunNominalTable` to print out a frequency table for nominal variables, 
## univariate and by province;
###############################################################################

FunNominal <- function(i, tab, group.by) {
  tab <- as.data.frame(tab,stringsAsFactors = FALSE)
  var <- tab[[i]]
  x <-  cbind(Total = table(var),
              table(var ,tab[,group.by]))
  if (!is.null(rownames(x)))  rownames(x)[rownames(x) == "101"] <- "Missing"
  prop.table(x, 2)*100
}

FunNominalTable <- function(i, tab, group.by){
  kable(FunNominal(i, tab, group.by), digits = 2, booktabs = TRUE, padding = 10)
}


## 2.2  Nominal variables - Barpllot 
## `FunNominalBarolot` to plot barolot for nominal variables, 
## univariate and by province;
###############################################################################

FunNominalBarplot <- function(i, tab, group.by){
  x <- FunNominal(i, tab, group.by)
  nr <- min(nrow(x)+3, 15)
  w <-  c(sum(table(tab[,group.by])), table(tab[,group.by]))
  par(mar = c(nr,5,1,1), xpd = TRUE)
  barplot(x, width = w, space = c(.5,.5,.1),
          horiz = TRUE,
          las=2,
          col = colorRampPalette(brewer.pal(11, "PiYG"))(nrow(x)),
          legend.text = rownames(x), args.legend = list(x = 0, y = 0,  xjust = 0))
}



## 3.1  Interval variables Print table 
## `FunIntervalTable` to print out a summary of intercal variables, 
## univariate and by province;
###############################################################################

FunInterval<- function(i, tab, group.by){
  tab <- as.data.frame(tab,stringsAsFactors = FALSE)
  var <- tab[[i]]
  group.summary <- tapply(var,tab[,group.by], summary)
  length(group.summary[[1]])<- max(sapply(group.summary, length))
  length(group.summary[[2]])<- max(sapply(group.summary, length))
  x <- cbind(Total = summary(var),
             group.summary[[1]],
             group.summary[[2]])
  colnames(x)[-1] <- names(group.summary)
  x
}

FunIntervalTable<- function(i, tab, group.by){
  kable(FunInterval(i, tab, group.by), digits = 2, booktabs = TRUE, padding = 10)
}

## 3.2  Interval variables - Boxplot
## `FunIntervalBoxplot` to plot boxplot for interval variables, 
## univariate and by province;
###############################################################################


FunIntervalBoxplot <- function(i, tab, group.by) {
  tab <- as.data.frame(tab,stringsAsFactors = FALSE)
  var <- tab[[i]]
  par(mar = c(5,3,1,1), xpd = TRUE)
  par(mfrow = c(1,2))
  if (!all(is.missing(var))) FunVioplot(list(as.vector(var[!is.missing(var)])), col = "rosybrown1", names = "Total")
  two <- split(as.vector(var[!is.missing(var)]), tab[,group.by][!is.missing(var)])
  
  two.non.missing <- two[!sapply(two, function(x) length(x))==0]
  two.non.missing.names <- names(two)[!sapply(two, function(x) length(x))==0]
  
  if (length(two.non.missing) != 0) FunVioplot(two.non.missing, col = "papayawhip", names = two.non.missing.names)
  
  par(mfrow = c(1,1))
}




## 4. consolidate all table and plot funcitons for household data
###############################################################################

FunTop <- function(i = 3, 
                   tab = ds.hohh,
                   group.by = "province"){
  cat("  \n")
  cat("## ", colnames(tab[i]), " -- ")
  cat(description(tab[[i]])) 
  cat("  \n")
  cat(annotation(tab[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(tab[[i]])[[3]])
  cat("  \n")
  cat("  \n")
  var <- tab[[i]]
  if (measurement(var) == "nominal") {
    x <- FunNominalTable(i, tab, group.by)} else {
      x <- FunIntervalTable(i, tab, group.by)}
  cat(x, sep = "\n")
  cat("  \n")
  if (measurement(var) == "nominal") {
    FunNominalBarplot(i, tab, group.by)} else {
      FunIntervalBoxplot(i, tab, group.by)}
  cat("  \n")
}


## 5. consolidate all table and plot funcitons for member data - 
###############################################################################

FunTopM <- function(i, group.by = ds.mmbr$province){
  cat("  \n")
  cat("## ", colnames(ds.mmbr[i]), " -- ")
  cat(description(ds.mmbr[[i]])) 
  cat("  \n")
  cat(annotation(ds.mmbr[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(ds.mmbr[[i]])[[3]])
  cat("  \n")
  cat("  \n")
  var <- ds.mmbr[[i]]
  if (measurement(var) == "nominal") {
    x <- FunNominalTable(var, group.by = group.by)} else {
      x <- FunIntervalTable(var,  group.by = group.by)}
  cat(x, sep = "\n")
  cat("  \n")
  if (measurement(var) == "nominal") {
    FunNominalBarplot(var, group.by = group.by)} else {
      FunIntervalBoxplot(var, group.by = group.by)}
  cat("  \n")
}

## 6. consolidate all table and plot funcitons for member data in HH, not respondent
###############################################################################

FunTopM.no.resp <- function(i, group.by = ds.mmbr$province){
  cat("  \n")
  cat("## ", colnames(ds.mmbr[i]), " -- ")
  cat(description(ds.mmbr[[i]])) 
  cat("  \n")
  cat(annotation(ds.mmbr[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(ds.mmbr[[i]])[[3]])
  cat("  \n")
  cat("  \n")
  var <- ds.mmbr[[i]][ds.mmbr$n4 != 1 & ds.mmbr$n1 == 1 ]
  group.by <- group.by[ds.mmbr$n4 != 1 & ds.mmbr$n1 == 1 ]
  if (measurement(var) == "nominal") {
    x <- FunNominalTable(var, group.by = group.by)} else {
      x <- FunIntervalTable(var,  group.by = group.by)}
  cat(x, sep = "\n")
  cat("  \n")
  if (measurement(var) == "nominal") {
    FunNominalBarplot(var, group.by = group.by)} else {
      FunIntervalBoxplot(var, group.by = group.by)}
  cat("  \n")
}

## 7. consolidate all table and plot funcitons for members not in HH
###############################################################################

FunTopM.C <- function(i, group.by = ds.mmbr$province){
  cat("  \n")
  cat("## ", colnames(ds.mmbr[i]), " -- ")
  cat(description(ds.mmbr[[i]])) 
  cat("  \n")
  cat(annotation(ds.mmbr[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(ds.mmbr[[i]])[[3]])
  cat("  \n")
  cat("  \n")
  var <- ds.mmbr[[i]][ds.mmbr$n1 != 1 ]
  group.by <- group.by[ds.mmbr$n1 != 1 ]
  if (measurement(var) == "nominal") {
    x <- FunNominalTable(var, group.by = group.by)} else {
      x <- FunIntervalTable(var,  group.by = group.by)}
  cat(x, sep = "\n")
  cat("  \n")
  if (measurement(var) == "nominal") {
    FunNominalBarplot(var, group.by = group.by)} else {
      FunIntervalBoxplot(var, group.by = group.by)}
  cat("  \n")
}

## 8 My vioplot
FunVioplot <- function(x, range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                       horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
                       lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
                       at, add = FALSE, wex = 1, drawRect = TRUE)  
{
  datas <- x
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  }
  else {
    label <- names
  }
  boxwidth <- 0.05 * wex
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), 
              c(base[[i]], rev(base[[i]])), col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty)
        rect(at[i] - boxwidth/2, q1[i], at[i] + boxwidth/2, 
             q3[i], col = rectCol)
        points(at[i], med[i], pch = pchMed, col = colMed)
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], 
                                              rev(at[i] + height[[i]])), col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
              lty = lty)
        rect(q1[i], at[i] - boxwidth/2, q3[i], at[i] + 
               boxwidth/2, col = rectCol)
        points(med[i], at[i], pch = pchMed, col = colMed)
      }
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, 
                 q1 = q1, q3 = q3))
}

## 9 Number of different generatiosn in household #############################
FunNumberGens <- function(test){
  if(is.na(test[1])){return(1)} else {
    return(length(unique(test[!is.na(test)])))
  }
}
