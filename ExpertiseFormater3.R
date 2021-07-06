library(tidyr)
library(ez)
library(ggplot2)
library(dplyr)


rm(list=ls())
setwd("C:/Users/Taren/Desktop/Output")
#Start writing output to a file
sink('MixedANOVA.txt')


#The eventual structure that the data will take after being run through the code
formatedData <- data.frame("SID" = double(),"Normal" = double(), "GCV" = double(), "imageType" = character(),"Group" = character())

#CHANGE REQUIRED BY CURRENT USER
#The number of radiologist subjects to analyze. Add additional subject numbers as data is collected
nsubjects <- c(102,103,104,105,106,107,108,109)


#Do analysis based on  these  conditions
targetPresence <- c(0,1)
imagetypes <- c("Radiograph","Perspective")
dependants <- c("Correct","CURRENT_SAC_AMPLITUDE","trialRT","dprime") #"IA_FIRST_FIXATION_TIME"


for (var in dependants) {
 
  for (pres in targetPresence){
    #first fixation is only relevant for target present trials
   # if (pres == 0 && var == "IA_FIRST_FIXATION_TIME"){
     # next()
   # }
    for (s in nsubjects){
      for(imageset in imagetypes){
        
        
        #import data and combine behavioral data into a single data frame
        #CHANGE REQUIRED BY CURRENT USER
        #Edit this to where your file is stored
        #you may also need to edit the names of the eye tracking data files (rawSacAmp and rawFirstFix)
        setwd("C:/Users/Taren/Desktop/RadData")
        rawBehav1 <- read.table(paste(s,"_naive_v1.txt", sep = ""), header = TRUE)
        rawBehav2 <- read.table(paste(s,"_naive_v2.txt", sep = ""), header = TRUE)
        #delete extra header if it is present in the data
        if (rawBehav2[1,1] == "snumber"){
          rawBehav2 <- rawBehav2[-c(1),]
        }
        if (rawBehav1[1,1] == "snumber"){
          rawBehav1 <- rawBehav1[-c(1),]
        }
        
        rawSacAmp <- read.table("sacamplituderadsthru8.txt", header = TRUE)
        rawFirstFix <- read.table("ttffradsthru8.txt", header = TRUE)
        rawBehav <- rbind(rawBehav1,rawBehav2)
        #convert to double. Will result in some warnings of NAs being introduced. That's okay as long as there is only a few.
        rawSacAmp$CURRENT_SAC_AMPLITUDE <- as.double(rawSacAmp$CURRENT_SAC_AMPLITUDE)
        #remove NA
        rawSacAmp <- rawSacAmp[!is.na(rawSacAmp$CURRENT_SAC_AMPLITUDE),]
        
        #filter image type to look at art and chest images separately 
        if (imageset == "Radiograph"){rawSacAmp <- filter(rawSacAmp, imageType == "chest")}
        if (imageset == "Perspective"){rawSacAmp <- filter(rawSacAmp, imageType == "art")}
        if (imageset == "Radiograph"){rawBehav <- filter(rawBehav, imageType == "chest")}
        if (imageset == "Perspective"){rawBehav <- filter(rawBehav, imageType == "art")}
        if (imageset == "Radiograph"){rawFirstFix <- filter(rawFirstFix, imageType == "chest")}
        if (imageset == "Perspective"){rawFirstFix <- filter(rawFirstFix, imageType == "art")}
        
        #filter out practice trials
        rawSacAmp <- filter(rawSacAmp, practice != 1)
        rawBehav <- filter(rawBehav, practice != 1)
        rawFirstFix <- filter(rawFirstFix, practice != 1)
        #filter to only look at one subject at a time
        rawSacAmp <- filter(rawSacAmp, snumber == s)
        rawFirstFix <- filter(rawFirstFix, snumber == s)
        #create a copy of behav data for dprime that isn't filtered on target pres
        rawdprime <- rawBehav
        #filter target presence
        rawSacAmp <- filter(rawSacAmp, targetPresent== pres)
        rawBehav <- filter(rawBehav, targetPresent == pres)
        rawFirstFix <- filter(rawFirstFix, targetPresent == pres)
        
        
        #choose what data to analyze based on the current dependent variable 
        if (var == "IA_FIRST_FIXATION_TIME"){currentData <- rawFirstFix}
        if (var == "CURRENT_SAC_AMPLITUDE") {currentData <- rawSacAmp}
        if (var == "Correct" || var == "trialRT") {currentData <- rawBehav}
        if (var == "dprime") {currentData <- rawdprime}
       
       
        
        if (var == "dprime"){
          var <- "Correct"
          
          normvaluesPres <- filter(currentData, viewType == "Normal")
          normvaluesPres <- filter(currentData, targetPresent == 1)
          normvaluesPres <- normvaluesPres$Correct
                                 
          normzscorePres <- (normvaluesPres - mean(normvaluesPres))/sd(normvaluesPres)
          normzscorePres <- scale(normvaluesPres)
            
          normvaluesAbs <- filter(currentData, viewType == "Normal")
          normvaluesAbs <- filter(currentData, targetPresent == 0)
          normvaluesAbs <- normvaluesPres$Correct
          
          normzscoreAbs <- (normvaluesAbs - mean(normvaluesAbs))/sd(normvaluesAbs)
          
          normdprime <- normzscorePres - normzscoreAbs
          
        }
        
        else{
        #adds each new row to a formatted data frame
        toAdd <- data.frame("SID" = s, 
                            "Normal" = mean(as.double(currentData %>% 
                                              filter(viewType == "Normal") %>%
                                              pull(var))), 
                            "GCV" = mean(as.double(currentData %>% 
                                           filter(viewType == "gazeContingent") %>%
                                           pull(var))), 
                            "imageType"= imageset, 
                            "Group" = "Radiologist")
        #add new row to the accumulating data frame
        formatedData <- rbind(formatedData, toAdd)
      }
        #resets the adding row after each row is added
        toAdd <- NULL
      }
    }


    if (pres == 1){
      #load in past target PRESENT data from naive an architects 
      outputname <- "(Target Present)"
      #choose what data to analyze based on the current dependent variable 
      if (var == "IA_FIRST_FIXATION_TIME"){
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/FirstFix.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/FirstFix.csv")
      }
      if (var == "CURRENT_SAC_AMPLITUDE") {
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/SacAmpPres.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/SacAmpPres.csv")
        ylabel <- "Visual Angle (deg.)"
      }
      if (var == "Correct") {
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/AccPres.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/AccPres.csv")
        ylabel <- "Proportion Correct"
      }
      if (var == "trialRT"){
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/RTPres.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/RTPres.csv")
        ylabel <- "Reaction time (s)"
      }
      
    }
    
    if (pres == 0){  
      #load in target ABSENT data
      outputname <- "(Target Absent)"
      #choose what data to analyze based on the current dependent variable 
      if (var == "CURRENT_SAC_AMPLITUDE") {
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/SacAmpAbs.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/SacAmpAbs.csv")
        ylabel <- "Visual Angle (deg.)"
      }
      if (var == "Correct") {
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/AccAbs.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/AccAbs.csv")
        ylabel <- "Proportion Correct"
      }
      if (var == "trialRT"){
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/RTAbs.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/RTAbs.csv")
        ylabel <- "Reaction time (s)"
      }
    }
    #format architect and naive data to work with the new radiologist data
    artdata$imageType <- "Perspective"
    chestdata$imageType <- "Radiograph"
    artdata$X <- NULL
    chestdata$X <- NULL
    
    setwd("C:/Users/Taren/Desktop/Output")
    #Does ANOVA analyses and exports to a .txt file
    
    #collects data for ANOVA
    wideNovRepArt<- artdata
    wideNovRepArt <- rbind(wideNovRepArt, filter(formatedData, imageType == "Perspective"))
    wideNovRepChest <- chestdata
    wideNovRepChest<- rbind(wideNovRepChest, filter(formatedData, imageType == "Radiograph"))
    wideNovRepArt$SID = as.factor(wideNovRepArt$SID)
    wideNovRepChest$SID = as.factor(wideNovRepChest$SID)
    
    #converts to long format for ANOVA
    longNovRepArt <- gather(wideNovRepArt, viewType, var, c(Normal, GCV), factor_key=TRUE)
    longNovRepChest <- gather(wideNovRepChest, viewType, var, c(Normal, GCV), factor_key=TRUE)
    
    #Does ANOVA on perspective images
    print(paste(var, "Perspective"))
    aov.NovRep = ezANOVA(data = longNovRepArt, dv = var, wid = SID, within = viewType, between = Group,
                         return_aov = FALSE)
    print(aov.NovRep,)
    #Does ANOVA on radiograph images
    print(paste(var,"Radiograph"))
    aov.NovRep = ezANOVA(data = longNovRepChest, dv = var, wid = SID, within = viewType, between = Group,
                         return_aov = FALSE)
    print(aov.NovRep)
    

  
    #add the radiologist data to the architect and naive data for graphing
    formatedData <- rbind(formatedData, artdata)
    formatedData <- rbind(formatedData, chestdata)
    
    #convert to long format for graph
    long <- pivot_longer(formatedData, c("Normal","GCV"), names_to = "viewType")
    long$condition <- paste(long$imageType , long$viewType)
    long$valueTypes <- paste(long$condition, long$Group)
    
    #calculate means and SE
    graph <- aggregate(long$value ~ long$valueTypes+long$Group+long$imageType+long$viewType, FUN= mean)
    error <- aggregate(long$value ~ long$valueTypes+long$Group+long$imageType+long$viewType, FUN= sd)
    colnames(error) <- c("condition","Group","ImageType","ViewType","sd")
    error$sd <- error$sd / sqrt(length(nsubjects))
    colnames(graph) <- c("condition","Group","ImageType","ViewType","value")
    graph$sterror <- error$sd
    #create new variables for the purpose of graphing
    graph$axis <- paste(graph$ImageType, graph$ViewType)
    graph$line <- paste(graph$Group, graph$ImageType)
    
    
    #create and save graph
    #WARNING every time you run this code the graphs will be overwritten without it asking you
    setwd("C:/Users/Taren/Desktop/Output")
    filename <- paste(var, outputname, ".pdf", sep = "")
    
    p <- ggplot(data = graph, aes(y = value, x =axis)) +
      geom_line(aes( group = line, color = Group)) + geom_point(size = 3, aes(color = Group)) +  
      geom_errorbar(aes(ymin = (value - sterror), ymax = (value + sterror), width = .1, color = Group)) +
      ylab(ylabel) +
      xlab("Image Type and Viewing Condition") +
      ggtitle(paste("Avg.",var, outputname))
    
    pdf(filename)
    print(p)
    dev.off()
    
    #Reset formatedData for next loop iteration
    formatedData <- NULL
    
  }
}
#stop writing to file
sink()



