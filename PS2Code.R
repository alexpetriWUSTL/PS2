#Problem 1 Part 1

vt <- c(1245, 925, 1622, 2101, 42) #Created a sample to test from (same numbers Montgomery used in class)

leemis <- function(x) { #Write a function 
  sigdig <- as.numeric(substr(x, 1, 1)) #Take substring of the first digit of each sample value
  i <- table(sigdig) #Create a table to find out how often each sigdig appears
  numtotals <- length(x) #Take the length to calculate total number of sigdigs
  x <- max(i/numtotals - log10(1 + 1/unique(sigdig))) #Set x of the function equal to a final value, Leemis' m stat
  return(x)
}

leemis(vt)

chogains <- function(d) { # Write a function
  sigdigC <- as.numeric(substr(d, 1, 1)) #
  iC <- table(sigdigC)
  numtotalsC <- length(d)
  d <- sqrt(sum((iC/numtotalsC - log10(1+1/unique(sigdig)))^2)))
  return(d)
}
chogains(vt)

