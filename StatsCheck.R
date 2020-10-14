install.packages("statcheck")
library("statcheck")
install.packages("readr") 
library(readr)

#the file needs to be in .txt format for this to work
txt <- read_file(file.choose())
statcheck(txt)