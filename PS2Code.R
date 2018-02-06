#Problem 1 Part 1

vt <- c(1245, 925, 1622, 2101, 42) #Created a sample to test from (same numbers Montgomery used in class)


benfordsLaw <- function(x, leemis, choGains) { #Write a function 
  sigdig <- as.numeric(substr(x, 1, 1)) #Take substring of the first digit of each sample value
  i <- table(sigdig) #Create a table to find out how often each sigdig appears
  numtotals <- length(x) #Take the length to calculate total number of sigdigs
    if (leemis == TRUE & choGains == TRUE) { #If statement - Boolean for user to call which statistic they want to calculate
      x <- max(i/numtotals - log10(1 + 1/unique(sigdig))) #Notate the Leemis stat using vectors previously set up
      d <- sqrt(sum((i/numtotals - log10(1+1/unique(sigdig)))^2)) #Notate the Cho-Gains stat using vectors previously set up
      return(list("Leemis's m" = x, "Cho-Gains' D" = d, "Digit Distribution" = i)) #Return a list of the statistics and digit distribution if user wants both stats
    }
    if (leemis == TRUE & choGains == FALSE){ #For if the user wants only the Leemis stat
      x <- max(i/numtotals - log10(1 + 1/unique(sigdig))) #Once again notate the Leemis stat
      return(list("Leemis's m" = x, "Digit Distribution" = i)) #Return list of the Leemis stat and digit distribution
    }
    if (choGains == TRUE & leemis == FALSE){ #For if the user wants only the Cho-Gains stat
      d <- sqrt(sum((i/numtotals - log10(1+1/unique(sigdig)))^2)) #Once again notate the Cho-Gains stat
      return(list("Cho-Gains' D" = d, "Digit Distribution" = i)) #Return list of the Cho-Gains stat and digit distribution
    }
}
benfordsLaw(vt, TRUE, TRUE) #Call function benfordsLaw using "vt", the digit vector I use to test my function, with answer
                            #true to both statistics


#Problem 2 - pander
leemis <- c(.954) #Leemis statistic to test
choGains <- c(1.222) #Cho-Gains statistic to test
astfunc <- function(x, statistic){ #function so i can couple my asterisks with the values
  choGainsThresh <- c(1.212, 1.330, 1.569)#create a vector with the Cho-Gains Threshold values of significance
  leemisThresh <- c(.851, .967, 1.212) #create a vector with the Leemis Threshold values of significance
    if (statistic == TRUE){ #True for when I want to assign asterisks to the choGains value
      if (x >= choGainsThresh[1] & x <= choGainsThresh[2]){ #Each if statement has the parameters of where the Cho-Gains value is
        return(paste(x, "*", sep=""))
      }
      if (x >= choGainsThresh[2] & x <= choGainsThresh[3]){
        return(paste(x, "**", sep=""))
      }
      if (x >= choGainsThresh[3]){
        return(paste(x, "***", sep=""))
      } 
      if (x < choGainsThresh[1]){
        return(x)
      }
    }
    if (statistic == FALSE){ #False for if i want to run the Leemis 
      if (x >= leemisThresh[1] & x <= leemisThresh[2]){
        return(paste(x, "*", sep=""))
      }
      if (x >= leemisThresh[2] & x <= leemisThresh[3]){
        return(paste(x, "**", sep=""))
      }
      if (x >= leemisThresh[3]){
        return(paste(x, "***", sep=""))
      } 
      if (x < leemisThresh[1]){
        return(x)
      }
    }
} 



install.packages("pander")
print.benfords <- function (leem, choGain){ #a function to print out the table with the Leemis and cho-Gains stat
  library("pander")
  leem <- astfunc(leem, FALSE) #vector with statistic and astfunc applied to it
  choGain <- astfunc(choGain, TRUE) #vector with statistic and astfunc applied
  table <- matrix(c(leem, choGain), ncol = 1, nrow = 2)
  colnames(table) <- c("Statistic and Significance")
  rownames(table) <- c("Leemis Statistic", "Cho-Gains Statistic")
  finalTable <- pandoc.table(table, caption = "p < .01 ***, p < .05 **, p < .10 *", plain.ascii = TRUE) #use pander to make a table with legend
  return(finalTable)
}
print.benfords(leemis, choGains) #return the final function

csvfunc <- function (leem, choGain){ #create a function to make the csv
  sink(file = "C:/Users/Alex Petri/Documents/benfordCSVv.csv", append = TRUE, split = FALSE) #use sink to put the file into my document folder
  sink(print.benfords(leemis, choGains))
}
csvfunc(leemis, choGains) #call function to make the file

















    
    