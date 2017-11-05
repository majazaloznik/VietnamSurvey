###############################################################################
## FUNCTIONS
###############################################################################
## 1.1  Swapping positions in the dataset.
## 2.1 Nominal variables Print table
## 2.2 Nominal variables barplot
## 4. consolidate all 2. or 3. functions, plus sugar:
###############################################################################

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

FunNominal <- function(var) {
  x <-  cbind(Total = table(var ),
              table(var ,ds.hohh$province))
  row_sub = apply(x, 1, function(row) !all(row == 0 ))
  x <- x[row_sub,]# remove rows with no cases at all
  #row_sub = row.names(x) != 101
  #x <- x[row_sub,]# remove rows with NA
  prop.table(x, 2)*100
}

FunNominalTable <- function(var){
  kable(FunNominal(var), digits = 2, booktabs = TRUE, padding = 10)
}


## 2.2  Nominal variables - Barpllot 
## `FunNominalBarolot` to plot barolot for nominal variables, 
## univariate and by province;
###############################################################################

FunNominalBarplot <- function(var) {
  w <-  c(nrow(ds.hohh), table(ds.hohh$province))
  par(mar = c(3,2,1,10), xpd = TRUE)
  barplot(FunNominal(var), width = w, space = c(.5,.5,.1),
          col = colorRampPalette(brewer.pal(11, "PiYG"))(nrow(FunNominal(var))),
          legend.text = rownames(FunNominal(var)), args.legend = list(x = 1200,  xjust = 0))
}


## 3.1  Interval variables Print table 
## `FunIntervalTable` to print out a summary of intercal variables, 
## univariate and by province;
###############################################################################

FunInterval<- function(var){
  provinces <- tapply(var, ds.hohh$province, summary)
  cbind(Total = summary(var),
        `Thai Binh` = provinces[[1]],
        `Vinh Phuc` = provinces[[2]])
}

FunIntervalTable<- function(var){
  kable(FunInterval(var), digits = 2, booktabs = TRUE, padding = 10)
}

## 3.2  Interval variables - Boxplot
## `FunIntervalBoxplot` to plot boxplot for interval variables, 
## univariate and by province;
###############################################################################

FunIntervalBoxplot <- function(var) {
  par(mar = c(5,3,1,1), xpd = TRUE)
  par(mfrow = c(1,2))
  boxplot(var, col = "rosybrown1", width = 2, xlab = "Total" )
  boxplot(var~ ds.hohh$province, col = "papayawhip", width = c(1,1))
  par(mfrow = c(1,1))
}


## 4. consolidate all 2. or 3. functions, plus sugar:
###############################################################################

FunTop <- function(i){
  cat("## ", colnames(ds.hohh[i] ), " -- ")
  cat(description(ds.hohh[[i]])) 
  cat("  \n")
  cat(annotation(ds.hohh[[i]])[[2]])
  cat("  \n")
  cat("From: ")
  cat(annotation(ds.hohh[[i]])[[3]])
  cat("  \n")
  var <- ds.hohh[[i]]
  if (measurement(var) == "nominal") {
  FunNominalTable(var)
  FunNominalBarplot(var)} else {
  FunIntervalTable(var)
  FunIntervalBoxplot(var)}
  cat("  \n")
}

