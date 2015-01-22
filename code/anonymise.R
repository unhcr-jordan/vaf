########################################################
## Anonymisation


## sdcMicro: Statistical Disclosure Control methods for anonymization of microdata and risk estimation
install.packages("sdcMicro")
install.packages("sdcMicroGUI") 

require(sdcMicroGUI); sdcGUI()



### http://stackoverflow.com/questions/10454973/how-to-create-example-data-set-from-private-data-replacing-variable-names-and-l
## A function to anonymise columns in 'colIDs' 
##    colIDs can be either column names or integer indices
anonymiseColumns <- function(df, colIDs) {
  id <- if(is.character(colIDs)) match(colIDs, names(df)) else colIDs
  for(id in colIDs) {
    prefix <- sample(LETTERS, 1)
    suffix <- as.character(as.numeric(as.factor(df[[id]])))
    df[[id]] <- paste(prefix, suffix, sep="")
  }
  names(df)[id] <- paste("V", id, sep="")
  df
}

## A data.frame containing sensitive information in column 1 & 2
## Anonymise it - df2 <- anonymiseColumns(df, c(1,3))

anonymise <- function(df, colString = "Variable", rowString = "Sample") {
  foo <- function(x) {
    if(is.factor(x)) {
      levels(x) <- sample(LETTERS, length(levels(x)))
    }
    x
  }
  ## replace the variable names
  colnames(df) <- paste(colString, seq_len(ncol(df)), sep = "")
  ## fudge any factor levels
  df <- data.frame(lapply(df, foo))
  ## replace rownames
  rownames(df) <- paste(rowString, seq_len(nrow(df)), sep = "")
  ## return
  df
}
