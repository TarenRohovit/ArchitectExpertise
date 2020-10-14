# Author Taren Rohovit
# Computes and formats mixed ANOVA tests to be included in the manuscript
# WARNING: DF are hard coded here to simply code, I cross checked every output value from this script
# with the values from my previous script. This script is intended as a formater first, and a way to double check
# outputs second. 



library(tidyr)
library(ez)

#write output ot file instead of printing in console
sink('MixedANOVAResultsFormated.txt')


#
#
#Does mixed ANOVA tests for Art images
#
#
cat("Art Images\n\n")
filenamelist <- c("AccPres", "AccAbs","dprime","RTPres","RTAbs","SacAmpPres","SacAmpAbs","FirstFix","DesTime")
for (val in filenamelist){
filename = paste("/Users/tarenrohovit/Desktop/pvalueArt/", val,".csv", sep = "")
wideNovRep<- as.data.frame(read.csv(file = filename, head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

#convert to long format
longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)
#do the analysis
aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
#Formats the output 
aov.NovRep<- data.frame(aov.NovRep)
cat(paste(val, "\n"))
x <- 1
while (x < 4) {
  cat(paste(aov.NovRep[x,1], " "))
  cat (paste("F(1,44)=", round(aov.NovRep[x,4], digits = 3), sep = ""))
  if (round(aov.NovRep[1,5], digits = 3) < 0.005)   {
    cat(", p<0.05")
  } 
  else{
    cat(paste(", p=", round(aov.NovRep[x,5], digits = 3)))
  }
  if (round(aov.NovRep[x,7], digits = 3) < 0.005)   {
    cat(", gn2<0.05\n")
  } 
  else{
    cat(paste(", gn2=", round(aov.NovRep[x,7], digits = 3),"\n", sep = ""))
  }   
  x <- x +1
}
}
cat("\n")

#
#
#Does mixed ANOVA tests for Chest images
#
#
cat("Chest Images\n\n")
filenamelist <- c("AccPres", "AccAbs","dprime","RTPres","RTAbs","SacAmpPres","SacAmpAbs","FirstFix","DesTime")
for (val in filenamelist){
  filename = paste("/Users/tarenrohovit/Desktop/pvalueRad/", val,".csv", sep = "")
  wideNovRep<- as.data.frame(read.csv(file = filename, head = TRUE))
  wideNovRep$SID = as.factor(wideNovRep$SID)
  
  #convert to long format
  longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)
  #do the analysis
  aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                       return_aov = FALSE)
  #Formats the output 
  aov.NovRep<- data.frame(aov.NovRep)
  cat(paste(val, "\n"))
  x <- 1
  while (x < 4) {
    cat(paste(aov.NovRep[x,1], " "))
    cat (paste("F(1,44)=", round(aov.NovRep[x,4], digits = 3), sep = ""))
    if (round(aov.NovRep[1,5], digits = 3) < 0.005)   {
      cat(", p<0.05")
    } 
    else{
      cat(paste(", p=", round(aov.NovRep[x,5], digits = 3)))
    }
    if (round(aov.NovRep[x,7], digits = 3) < 0.005)   {
      cat(", gn2<0.05\n")
    } 
    else{
      cat(paste(", gn2=", round(aov.NovRep[x,7], digits = 3),"\n", sep = ""))
    }   
    x <- x +1
  }
}
cat("\n")

#stops wiritng to a file
sink()
