# Operation Span - data extraction
# Jason Gullifer - 10/11/2011
#library(ggplot2)

#for (file in dir(pattern="_coded.csv")){
load_filename <- "datacoded_granada.csv"
subject_data_filename <- paste("subject_data_",load_filename,sep="") 
data <- read.csv(load_filename)

userinput <- readline("(1) Use RA codings (2) Use REGEX codings?\n")
if (userinput == "1"){
  print("Using RA codings.")
  recall_column <- "Recalled"
  print("Calculating intrusions.")
  intrusions <- 1
}else{
  print("Using REGEX codings.")
  recall_column<- "CompRecalled"
  print("Not calculating intrusions.")
  intrusions <- 0
}

# Remove practice trials
data <- data[grep("Prac*", data$Running, invert=T),]

# Remove the recall trials
data <- data[grep("Recall*", data$Running, invert=T),]

# Some blocks were named Blcok, change them to Block
data$Running <- gsub("Blcok", "Block", data$Running)

# Refactor and Rename columns
data$Running <- factor(data$Running)
data$answer <- factor(data$answer)

# data.correct is only trials where the math was correct
data.correct <- data[data$stimulus1.ACC==1,]

# Data visualization.
#ggplot(data.correct, aes(x=stimulus1.RT)) + geom_density(aes(fill=answer))
#ggplot(data.correct, aes(y=stimulus1.RT, x=answer)) + geom_boxplot()

# No slow outlier corrections needed. If anything, the trials moved too
# fast and cut off usable reaction times. There can be premptive
# strikes. Best to remove anything faster than 200ms

# Remove any fast outliers
data.correct <- data.correct[data.correct$stimulus1.RT>=200,]

if (intrusions==1){
# Melt data.correct with recalled, rt, and intrusions as measure vars
data.correct.m <- melt(data.correct,
                       measure.vars = c(recall_column,
                         "stimulus1.RT",
                         "Intrusions"))

# Get Ospan Score (number of correct and recalled trials), also get
# number of intrusions by subject
ospan_data <- cast(data.correct.m,
                   Subject~variable,
                   sum,
                   subset = variable == recall_column | variable == "Intrusions")
colnames(ospan_data) <- c("Subject", "Ospan.Recall", "Intrusions")

}else{
# Melt data.correct with recalled, rt, and intrusions as measure vars  
  data.correct.m <- melt(data.correct,
                       measure.vars = c(recall_column,
                         "stimulus1.RT"))

# Get Ospan Score (number of correct and recalled trials)
ospan_data <- cast(data.correct.m,
                   Subject~variable,
                   sum,
                   subset = variable == recall_column)
colnames(ospan_data) <- c("Subject", "Ospan.Recall")

}

# Get RT by subject and by trial type (yes/no)
rt_data <- cast(data.correct.m,
                Subject ~ answer + variable,
                mean,
                subset = variable == "stimulus1.RT",
                margins = "grand_col")
colnames(rt_data) <- c("Subject","no_RT","yes_RT","meanRT")

# Melt original data for calculation math accuracy
data.m<-melt(data, measure.vars = c("stimulus1.ACC"))

# Get math accuracy data by subject
math_acc_data <- cast(data.m, Subject ~ variable, sum)
colnames(math_acc_data) <- c("Subject", "MathCorrect")

# Merge ospan data, math data, and rt data together
subject_data <- merge(ospan_data, math_acc_data, by = "Subject")
subject_data <- merge(subject_data, rt_data, by = "Subject")

# Output data
write.csv(subject_data, subject_data_filename,row.names=F)
#}
