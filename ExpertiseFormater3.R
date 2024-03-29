# Author Taren Rohovit, July, 2021 tren63@gmail.com 
#READ ME BEFORE USING THIS CODE
# 1) make sure that you have all 4 libraries listed below installed (tidyr, ez, ggplot2, and dplyr)
# 2) ensure you have a folder named "Output" stored on your desktop. This is where the output of this code will be stored
# 3) ensure you have folders "pvalueArt" and "pvalueRad" on your desktop. These contain the data from experiment 1. email me if you're missing this
# 4) ensure you have a folder named "RadData" on your desktop containg the data from the radiologists
# 5) change the file path on code line 24 to your path
# 6) change the file path on code line 57 to your path
# 7) on code line 68 and 69 change the name of the file to what you named your eyetracking data
# 8) on code lines 139-190 change the file path name to yours
# 9) on code line 200 change the file path to yours
# 10) on code line 264 change the file path to yours
# 11) on lines 219-226 you're able to change which groups are compared against each other in the ANOVA
# 12) on line 34 you will need to add the subject ID of each radiologist subject


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
dependants <- c("Correct","CURRENT_SAC_AMPLITUDE","trialRT","IA_FIRST_FIXATION_TIME","Decision_Time")


for (var in dependants) {
 
  for (pres in targetPresence){
    #first fixation is only relevant for target present trials
    if (pres == 0 && (var == "IA_FIRST_FIXATION_TIME" || var =="Decision_Time")){
      next()
    }
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
        rawFirstFix <- read.table("RadFirstFix.txt", header = TRUE)
        rawBehav <- rbind(rawBehav1,rawBehav2)
        rawFirstFix <- filter(rawFirstFix, IA_LABEL == "TARGET_LOCATION")
        
        #convert to double. Will result in some warnings of NAs being introduced. That's okay as long as there is only a few.
        rawSacAmp$CURRENT_SAC_AMPLITUDE <- as.double(rawSacAmp$CURRENT_SAC_AMPLITUDE)
        rawFirstFix$IA_FIRST_FIXATION_TIME <- as.double(rawFirstFix$IA_FIRST_FIXATION_TIME)
        #remove NA
        rawSacAmp <- rawSacAmp[!is.na(rawSacAmp$CURRENT_SAC_AMPLITUDE),]
        rawFirstFix <- rawFirstFix[!is.na(rawFirstFix$IA_FIRST_FIXATION_TIME),]
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
        #calculate time to first fixation and make a new column for it
        rawFirstFix$IA_FIRST_FIXATION_TIME <- rawFirstFix$IA_FIRST_FIXATION_TIME /1000
        rawFirstFix$trialRT <- rawFirstFix$trialRT/1000
        rawFirstFix$Decision_Time <- (rawFirstFix$trialRT - rawFirstFix$IA_FIRST_FIXATION_TIME)
        
        #choose what data to analyze based on the current dependent variable 
        if (var == "IA_FIRST_FIXATION_TIME" || var == "Decision_Time"){currentData <- rawFirstFix}
        if (var == "CURRENT_SAC_AMPLITUDE") {currentData <- rawSacAmp}
        if (var == "Correct" || var == "trialRT") {currentData <- rawBehav}
        if (var == "dprime") {currentData <- rawdprime}
       
       
        
 
      
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
        ylabel <- "Time (s)"
        artdata$Normal <- artdata$Normal /1000
        artdata$GCV <- artdata$GCV /1000
        chestdata$Normal <- chestdata$Normal /1000
        chestdata$GCV <- chestdata$GCV / 1000
      }
      if (var == "Decision_Time"){
        artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/DesTime.csv")
        chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/DesTime.csv")
        ylabel <- "Time (s)"
        artdata$Normal <- artdata$Normal /1000
        artdata$GCV <- artdata$GCV /1000
        chestdata$Normal <- chestdata$Normal /1000
        chestdata$GCV <- chestdata$GCV / 1000
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
    
    longNovRepArt$SID <- as.factor(longNovRepArt$SID)
    longNovRepChest$SID <- as.factor(longNovRepChest$SID)
    
    #Use these filters to control what groups are compared against each other in the ANOVA 
    #The group that is uncommented is the one that is EXCLUDED from analysis.
    #To change just comment and uncomment accordingly
    #longNovRepArt <- filter(longNovRepArt, Group != "Naive")
    #longNovRepArt <- filter(longNovRepArt, Group != "Architect")
    longNovRepArt <- filter(longNovRepArt, Group != "Radiologist")
    #longNovRepChest <- filter(longNovRepChest, Group != "Naive")
    #longNovRepChest <- filter(longNovRepChest, Group != "Architect")
    longNovRepChest <- filter(longNovRepChest, Group != "Radiologist")
    
    #Does ANOVA on perspective images
    print(paste(outputname, var, "Perspective"))
    aov.NovRep = ezANOVA(data = longNovRepArt, dv = var, wid = SID, within = viewType, between = Group,
                         return_aov = FALSE)
    print(aov.NovRep,)
    #Does ANOVA on radiograph images
    print(paste(outputname, var,"Radiograph"))
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



