###############################################################################
## FUNCTIONS
###############################################################################
## 1.1  Swapping positions in the dataset.
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

