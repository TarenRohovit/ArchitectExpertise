library(tidyr)
library(ez)
library(ggplot2)
library(dplyr)


rm(list=ls())

#The eventual structure that the data will take after being run through the code
formatedData <- data.frame("SID" = double(),"Normal" = double(), "GCV" = double(), "imageType" = character(),"Group" = character())

#CHANGE REQUIRED BY CURRENT USER
#The number of radiologist subjects to analyze. Add additional subject numbers as data is collected
nsubjects <- c(102,103,104,105,106,107,108,109)


#Do analysis based on  these  conditions
targetPresence <- c(0,1)
imagetypes <- c("Radiograph","Perspective")
dependants <- c("clickResponse","CURRENT_SAC_AMPLITUDE","trialRT","IA_FIRST_FIXATION_TIME","dprime","DesTime")

#
#
#
#Saccadic Amplitude - Creates graphs comparing the saccadic amplitude of each population to each other
#
#
#
for (pres in targetPresence){
  for (s in nsubjects){
    for(imageset in imagetypes){
      #import data and combine behavioral data into a single data frame
      
      #CHANGE REQUIRED BY CURRENT USER
      #Edit this to where your file is stored
      setwd("C:/Users/Taren/Desktop/RadData")
      rawBehav1 <- read.table(paste(s,"_naive_v1.txt", sep = ""), header = TRUE)
      rawBehav2 <- read.table(paste(s,"_naive_v2.txt", sep = ""), header = TRUE)
      rawSacAmp <- read.table("sacamplituderadsthru8.txt", header = TRUE)
      rawBehav <- rbind(rawBehav1,rawBehav2)
      #convert to double. Will result in some warnings of NAs being introduced. That's okay as long as there is only a few.
      rawSacAmp$CURRENT_SAC_AMPLITUDE <- as.double(rawSacAmp$CURRENT_SAC_AMPLITUDE)
      #remove NA
      rawSacAmp <- rawSacAmp[!is.na(rawSacAmp$CURRENT_SAC_AMPLITUDE),]
  
      #filter out practice trials
      rawSacAmp <- filter(rawSacAmp, practice != 1)
      rawBehav <- filter(rawBehav, practice != 1)
      #filter to only look at one subject at a time
      rawSacAmp <- filter(rawSacAmp, snumber == s)
      #filter target presence
      rawSacAmp <- filter(rawSacAmp, targetPresent== pres)
      #filter image type to look at art and chest images separately 
      if (imageset == "Radiograph"){rawSacAmp <- filter(rawSacAmp, imageType == "chest")}
      if (imageset == "Perspective"){rawSacAmp <- filter(rawSacAmp, imageType == "art")}
      
     
      #adds each new row to a formatted data frame
      toAdd <- data.frame("SID" = s, 
                          "Normal" = mean(rawSacAmp$CURRENT_SAC_AMPLITUDE[rawSacAmp$viewType == "Normal"]), 
                          "GCV" = mean(rawSacAmp$CURRENT_SAC_AMPLITUDE[rawSacAmp$viewType == "gazeContingent"]), 
                          "imageType"= imageset, 
                          "Group" = "Radiologist")
      #add new row to the accumulating data frame
      formatedData <- rbind(formatedData, toAdd)
      #resets the adding row after each row is added
      toAdd <- NULL
    }
  }
  
  #
  #TODO
  #eventually do ANOVA here in both if statements and output ANOVA results 
  #
  if (pres == 1){
    #load in past target PRESENT data from naive an architects 
    artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/SacAmpPres.csv")
    chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/SacAmpPres.csv")
    outputname <- "(Target Present)"
  }
    
  if (pres == 0){  
    #load in target ABSENT data
    artdata <-read.csv("C:/Users/Taren/Desktop/pvalueArt/SacAmpAbs.csv")
    chestdata <-read.csv("C:/Users/Taren/Desktop/pvalueRad/SacAmpAbs.csv")
    outputname <- "(Target Absent)"
  }
    #format architect and naive data to work with the new radiologist data
    artdata$imageType <- "Perspective"
    chestdata$imageType <- "Radiograph"
    artdata$X <- NULL
    chestdata$X <- NULL
    #add the radiologist data to the architect and naive data
    formatedData <- rbind(formatedData, artdata)
    formatedData <- rbind(formatedData, chestdata)
    targetPresent <- formatedData
  
    #convert to long format
    long <- pivot_longer(targetPresent, c("Normal","GCV"), names_to = "viewType")
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
    filename <- paste("Avg. Saccadic Amplitude", outputname, ".pdf", sep = "")
    
    p <- ggplot(data = graph, aes(y = value, x =axis)) +
      geom_line(aes( group = line, color = Group)) + geom_point(size = 3, aes(color = Group)) +  
      geom_errorbar(aes(ymin = (value - sterror), ymax = (value + sterror), width = .1, color = Group)) +
      ylab("Visual Angle (deg.)") +
      xlab("Image Type and Viewing Condition") +
      ggtitle(paste("Avg. Saccadic Amplitude", outputname))
    
    pdf(filename)
    print(p)
    dev.off()
    
    #Reset formatedData for next loop iteration
    formatedData <- NULL

}









