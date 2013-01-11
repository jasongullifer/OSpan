# Operation Span -  coding
# Jason Gullifer - 10/11/2011
library(reshape)

#for (file in dir(pattern='*.txt')){
load_filename <- "ls_ospan_missing4_fixed.txt"
coded_filename = paste(strsplit(load_filename,".txt"),"_coded.csv",sep="") 
data <- read.delim(load_filename, skip=2, encoding="latin1")

# Remove practice trials
data <- data[grep("Prac*|Recallp", data$Running, invert=T),]
data$Running <- gsub("Blcok", "Block", data$Running)

# Refactor and Rename columns
data$Running <- factor(data$Running)
data$answer <- factor(data$answer)

#Make a blocking column from Running column
data$Block<-as.numeric(gsub("Recall|Block","",data$Running))

#Reduplicate the InputRecall column for each block of each subject
get_input<-function(df){transform(df,Input=as.character(df$InputRecall.RESP[nrow(df)]))}
data<-ddply(data,.(Subject,Block), get_input ,.progress="text")

#Remove recall trials
data <- data[grep("Recall*", data$Running, invert=T),]
data$Running <- factor(data$Running)

# Get rid of random keypresses
data$Input<-gsub("\\{SPACE\\}"," ",data$Input)
data$Input<-gsub("\\{,\\}"," ",data$Input)
data$Input<-gsub("\\{ENTER\\}"," ",data$Input)
data$Input<-gsub("\\{ESCAPE\\}","",data$Input)
data$Input<-gsub("\\{;\\}","n",data$Input) #this is spanish keyboard, probably change
data$Input<-gsub("\\{.*?\\}","",data$Input)

# Get rid of special characters for the Spanish version
data$stimulus2 <- gsub("ó","o",data$stimulus2)
data$stimulus2 <- gsub("á","a",data$stimulus2)
data$stimulus2 <- gsub("ñ","n",data$stimulus2)
data$stimulus2 <- gsub("í","i",data$stimulus2)

#Grep through each row to find recall accuracy. Could use agrep
#function if you want to allow typos, but this may do more harm than
#good.
data$CompRecalled <- 0
for (i in 1:nrow(data)){
  current_grep <- grep(paste("([[:blank:]]|^)",data$stimulus2[i],"([[:blank:]]|$)",sep=""), data$Input[i],ignore.case=T)
  if(length(current_grep>0)){
    data$CompRecalled[i] = current_grep
}else{
    data$CompRecalled[i] = 0
}
}

#Write the data
write.csv(data,coded_filename,row.names=F)
#}
