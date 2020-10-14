library(tidyr)
library(ez)

#write output ot file instead of printing in console
sink('ScanPath.txt')

#Does mixed ANOVA test for scanpath on Art images
#Read in data, converting to a data frame and convert SID into a factor

wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/artscan.csv", head = TRUE))
naivewideNovRep<- as.data.frame(read.csv(file= "/Users/tarenrohovit/Desktop/naiveartscan.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)
naivewideNovRep$SID = as.factor(naivewideNovRep$SID)
wideNovRep <- rbind(wideNovRep, naivewideNovRep)
#removes subjects with bad eyetracking calibration
wideNovRep <- subset(wideNovRep, SID != 56 & SID != 43 & SID != 60 & SID != 62 & SID!=82 & SID != 67 & SID != 48 & SID != 53 & SID != 55 & SID != 48 & SID !=53 & SID !=55)
#convert to long format
longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)
#do the analysis
aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                      return_aov = FALSE)
print("Scan Path Ratio Art")
print(aov.NovRep)

#Does mixed ANOVA test for scanpath on Chest images
#Read in data, converting to a data frame and convert SID into a factor

wideNovRep<- as.data.frame(read.csv(file = "/Users/tarenrohovit/Desktop/chestscan.csv", head = TRUE))
naivewideNovRep<- as.data.frame(read.csv(file= "/Users/tarenrohovit/Desktop/naivechestscan.csv", head = TRUE))
wideNovRep$SID = as.factor(wideNovRep$SID)
naivewideNovRep$SID = as.factor(naivewideNovRep$SID)
wideNovRep <- rbind(wideNovRep, naivewideNovRep)
#removes subjects with bad eyetracking calibration
wideNovRep <- subset(wideNovRep, SID != 56 & SID != 43 & SID != 60 & SID != 62 & SID!=82 & SID != 67 & SID != 48 & SID != 53 & SID != 55 & SID != 48 & SID !=53 & SID !=55)
#convert to long format
longNovRep <- gather(wideNovRep, viewType, rt, c(Normal, GCV), factor_key=TRUE)
#do the analysis
aov.NovRep = ezANOVA(data = longNovRep, dv = rt, wid = SID, within = viewType, between = Group,
                     return_aov = FALSE)
print("Scan Path Ratio Chest")
print(aov.NovRep)


#Stop writing to file
sink()