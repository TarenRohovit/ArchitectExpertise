library(tidyr)
library(ez)

#write output ot file instead of printing in console
sink('MixedANOVAResultsRadCorrectFinal.txt')

#Does mixed ANOVA test for RT (targets present, absent, and both)
#Read in data, converting to a data frame and convert SID into a factor

#Absent
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/RTAbs.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)
#convert to long format
longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)
#do the analysis
aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                      return_aov = FALSE)
print("RT absent")
print(aov.NovRep)

#Present
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/RTPres.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("RT present")
print(aov.NovRep)





#
#Does mixed ANOVA test for Accuracy (targets present, absent, and both)
#
#

#Absent
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/AccAbs.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, accuracy, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = accuracy, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Accuracy Absent")
print(aov.NovRep)

#Present
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/AccPres.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, accuracy, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = accuracy, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Accuracy Present")
print(aov.NovRep)


#
#Does mixed ANOVA test for saccadic Amplitude (targets present, absent, and both)
#
#
#Absent
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/SacAmpAbs.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, sacAmp, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = sacAmp, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Saccadic Amplitude Absent")
print(aov.NovRep)

#Present
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/SacAmpPres.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, sacAmp, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = sacAmp, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Saccadic Amplitude Present")
print(aov.NovRep)



#
#Does mixed ANOVA test for d-prime
#
#
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/dprime.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, dprime, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = dprime, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("d-prime")
print(aov.NovRep)

#
#Does mixed ANOVA test for Decision Time
#
#
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/DesTime.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, desTime, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = desTime, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("decision Time")
print(aov.NovRep)


#
#Does mixed ANOVA test for time to first fixation
#
#
wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/pvalueRad/FirstFix.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)

longNovRep <- gather(wideNovRep, viewType, firstfix, c(Normal, GCV), factor_key=TRUE)

aov.NovRep = ezANOVA(data = longNovRep, dv = firstfix, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Time to first fixation")
print(aov.NovRep)

#Stop writing to file
sink()