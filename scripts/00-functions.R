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
###############################################################################

## 0. preliminaires
###############################################################################
require(RColorBrewer)
library(vioplot)

# 1.1 Swapping positions in the dataset. Used in:
###############################################################################
FunSwap <- function(ds , New, After) {# only use to move back!!
  index <- 1:ncol(ds)
  from <- which(names(ds)[index] == New)
  to <- which(names(ds)[index] == After)
  index <- append(index, values = from, after = to) # insert in new position
  index <- index[-(from+1)] # remove from old
  return(ds[index])
}


## 2.1  Nominal variables Print table 
## `FunNominalTable` to print out a frequency table for nominal variables, 
## univariate and by province;
###############################################################################
FunNominal <- function(var, group.by = ds.hohh$province) {
  x <-  cbind(Total = table(var ),
              table(var ,group.by))
  # row_sub = apply(x, 1, function(row) !all(row == 0 ))
  # x <- x[row_sub,]# remove rows with no cases at all
  if (!is.null(rownames(x)))  rownames(x)[rownames(x) == "101"] <- "Missing"
  prop.table(x, 2)*100
}


FunNominalTable <- function(var, group.by){
  kable(FunNominal(var, group.by), digits = 2, booktabs = TRUE, padding = 10)
}


## 2.2  Nominal variables - Barpllot 
## `FunNominalBarolot` to plot barolot for nominal variables, 
## univariate and by province;
###############################################################################

FunNominalBarplot <- function(var, group.by = ds.mmbr$province) {
  nr <- min(nrow(FunNominal(var, group.by = group.by))+3, 15)
  w <-  c(sum(table(group.by)), table(group.by))
  par(mar = c(nr,5,1,1), xpd = TRUE)
  barplot(FunNominal(var, group.by = group.by), width = w, space = c(.5,.5,.1),
          horiz = TRUE,
          las=2,
          col = colorRampPalette(brewer.pal(11, "PiYG"))(nrow(FunNominal(var, group.by = group.by))),
          legend.text = rownames(FunNominal(var, group.by =group.by)), args.legend = list(x = 0, y = 0,  xjust = 0))
}



## 3.1  Interval variables Print table 
## `FunIntervalTable` to print out a summary of intercal variables, 
## univariate and by province;
###############################################################################

FunInterval<- function(var, group.by = ds.hohh$province){
  group.summary <- tapply(var,group.by, summary)
  length(group.summary[[1]])<- max(sapply(group.summary, length))
  length(group.summary[[2]])<- max(sapply(group.summary, length))
  x <- cbind(Total = summary(var),
             group.summary[[1]],
             group.summary[[2]])
  colnames(x)[-1] <- names(group.summary)
  x
}

FunIntervalTable<- function(var, group.by){
  kable(FunInterval(var, group.by), digits = 2, booktabs = TRUE, padding = 10)
}

## 3.2  Interval variables - Boxplot
## `FunIntervalBoxplot` to plot boxplot for interval variables, 
## univariate and by province;
###############################################################################


FunIntervalBoxplot <- function(var, group.by = ds.hohh$province) {
  par(mar = c(5,3,1,1), xpd = TRUE)
  par(mfrow = c(1,2))
  if (!all(is.missing(var))) vioplot(as.vector(var[!is.missing(var)]), col = "rosybrown1", names = "Total")
  two <- split(as.vector(var[!is.missing(var)]), group.by[!is.missing(var)])
  if (length(two) == 2)  {vioplot(two[[1]], two[[2]], col = "papayawhip", names = names(two))} else {
    if (length(two) == 1) vioplot(two[[1]], col = "papayawhip", names = names(two))}
  par(mfrow = c(1,1))
}




## 4. consolidate all table and plot funcitons for household data
###############################################################################

FunTop <- function(i){
  cat("  \n")
  cat("## ", colnames(ds.hohh[i] ), " -- ")
  cat(description(ds.hohh[[i]])) 
  cat("  \n")
  cat(annotation(ds.hohh[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(ds.hohh[[i]])[[3]])
  cat("  \n")
  cat("  \n")
  var <- ds.hohh[[i]]
  if (measurement(var) == "nominal") {
    x <- FunNominalTable(var)} else {
      x <- FunIntervalTable(var)}
  cat(x, sep = "\n")
  cat("  \n")
  if (measurement(var) == "nominal") {
    FunNominalBarplot(var)} else {
      FunIntervalBoxplot(var)}
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

