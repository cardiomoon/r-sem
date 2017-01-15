dictionary=read.csv("dictionary.csv",fileEncoding="utf-8") 
# update the processed translation file translation.bin
# run this every time dictionary.csv is updated 
# it reads the look-up table in dictionary.csv and turns it into a 2D list

library(plyr)
translationContent <- read.csv("dictionary.csv",fileEncoding="utf-8") 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))

save(translation, file = "translation.bin")


