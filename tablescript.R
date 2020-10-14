# Author: Taren Rohovit
#
# Creates a table of means and standard deviations from a collection of csv files containing data
#
#write output to a file instead of the console
sink('ArtTable.txt')

cat("Art Images\n\n")

#initializes the data frame
filenamelist <- c("AccPres", "AccAbs","dprime","SacAmpPres","SacAmpAbs","RTPres","RTAbs", "FirstFix","DesTime")
varnames <- c("ARC_GCV_M","ARC_GCV_SD","ARC_NOR_M","ARC_NOR_SD","NAI_GCV_M","NAI_GCV_SD","NAI_NOR_M","NAI_NOR_SD")
newtable <- data.frame(matrix(nrow = 9, ncol = 8))
row.names(newtable) <- filenamelist
colnames(newtable) <- varnames

#count controls for where the computed means and SD should be placed in the table
count <- 1

#computes means and SD as well as placeing the computed values inside the table
for (val in filenamelist){
  
  #Opens the file and converts the csv file into a dataframe
  fileToOpen = paste("/Users/tarenrohovit/Desktop/pvalueArt/", val,".csv", sep = "")
  filename <- as.data.frame(read.csv(file = fileToOpen, head = TRUE, stringsAsFactors = FALSE))
  
  #splits the csv file into two dataframes containing either archietcts or naive subjects only
  Architects <-subset(filename, grepl("Architect", filename$Group))
  Naives <- subset(filename, grepl("Naive", filename$Group))
  
  #Corrects for a difference in units in the raw data
  if (identical(val,"FirstFix") | identical(val,"DesTime")){
    Architects$GCV <- Architects$GCV / 1000
    Architects$Normal <-Architects$Normal / 1000
    Naives$GCV <- Naives$GCV / 1000
    Naives$Normal <- Naives$Normal / 1000
  }

  #computes the values and inputs them into the table
  newtable[count,1] <- round(mean(Architects$GCV), digits = 2)
  newtable[count,2] <- round(sd(Architects$GCV), digits = 2)
  newtable[count,3] <- round(mean(Architects$Normal), digits = 2)
  newtable[count,4] <- round(sd(Architects$Normal), digits = 2)
  newtable[count,5] <- round(mean(Naives$GCV), digits = 2)
  newtable[count,6] <- round(sd(Naives$GCV), digits = 2)
  newtable[count,7] <- round(mean(Naives$Normal), digits = 2)
  newtable[count,8] <- round(sd(Naives$Normal), digits = 2)
  
  #increase count on each loop iteration 
  count <- count + 1
}

#print the table
cat(newtable)

#stops writing to a file
sink()