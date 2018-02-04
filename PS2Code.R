#Problem 1 Part 1

vt <- c(1245, 925, 1622, 2101, 42) #Created a sample to test from (same numbers Montgomery used in class)


benfordsLaw <- function(x, leemis, choGains) { #Write a function 
  sigdig <- as.numeric(substr(x, 1, 1)) #Take substring of the first digit of each sample value
  i <- table(sigdig) #Create a table to find out how often each sigdig appears
  numtotals <- length(x) #Take the length to calculate total number of sigdigs
    if (leemis == TRUE & choGains == TRUE) { #If statement - Boolean for user to call which statistic they want to calculate
      x <- max(i/numtotals - log10(1 + 1/unique(sigdig))) #Notate the Leemis stat using vectors previously set up
      d <- sqrt(sum((i/numtotals - log10(1+1/unique(sigdig)))^2)) #Notate the Cho-Gains stat using vectors previously set up
      return(list("Leemis's m" <- x, "Cho-Gains' D" <- d, "Digit Distribution" <- i)) #Return a list of the statistics and digit distribution if user wants both stats
    }
    if (leemis == TRUE & choGains == FALSE){ #For if the user wants only the Leemis stat
      x <- max(i/numtotals - log10(1 + 1/unique(sigdig))) #Once again notate the Leemis stat
      return(list("Leemis's m" <- x, "Digit Distribution" <- i)) #Return list of the Leemis stat and digit distribution
    }
    if (choGains == TRUE & leemis == FALSE){ #For if the user wants only the Cho-Gains stat
      d <- sqrt(sum((i/numtotals - log10(1+1/unique(sigdig)))^2)) #Once again notate the Cho-Gains stat
      return(list("Cho-Gains' D" <- d, "Digit Distribution" <- i)) #Return list of the Cho-Gains stat and digit distribution
    }
}
benfordsLaw(vt, TRUE, TRUE) #Call function benfordsLaw using "vt", the digit vector I use to test my function, with answer
                            #true to both statistics


#Problem 2
leemis <- c(.299) #Leemis statistic to test
choGains <- c(.1299) #Cho-Gains statistic to test
print.bedfords <- function(x){ #Set up my print.bedfords function
  leemisThresh <- c(.851, .967, 1.212) #create a vector with the Leemis Threshold values of significance
  choGainsThresh <- c(1.212, 1.330, 1.569)#create a vector with the Cho-Gains Threshold values of significance
  if (!is.na(leemis)){ #If statement to test if there is an available value for the Leemis statistic
    if (leemis >= leemisThresh[1] & leemis <= leemisThresh[2]){
      print(paste(leemis, "*", sep=""))
    }
    if (leemis >= leemisThresh[2] & leemis <= leemisThresh[3]){
      print(paste(leemis, "**", sep=""))
    }
    if (leemis >= leemisThresh[3]){
      print(paste(leemis, "***", sep=""))
    } 
    if (leemis < leemisThresh[1]) {
      print(leemis)
    }
  }
  if (!is.na(choGains)){ #if statement to test if there is an available value for Cho-Gains
    if (choGains >= choGainsThresh[1] & choGains <= choGainsThresh[2]){
      print(paste(choGains, "*", sep=""))
    }
    if (choGains >= choGainsThresh[2] & choGains <= choGainsThresh[3]){
      print(paste(choGains, "**", sep=""))
    }
    if (choGains >= choGainsThresh[3]){
      print(paste(choGains, "***", sep=""))
    } 
    if (choGains < choGainsThresh[1]) {
      print(choGains)
    }
  }
}
print.bedfords(list("Leemis" <- leemis, "Cho-Gains" <- choGains))) #call the function for my leemis and choGains test values above


    
    
    
    