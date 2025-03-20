#install.packages(c("tidyverse","moments","ggpubr"))  #De-comment the code to install
library("tidyverse"); library(moments); library(ggpubr)

#Import the dataset into R Studio.
dat <- read.csv("WACY-COM.csv", na.strings=NA, stringsAsFactors=TRUE)
set.seed(10657323)  #Pass student ID as parameter to seed function to make the dataset unique

#Randomly select 400 rows
selected.rows <- sample(1:nrow(dat),size=400,replace=FALSE)

#Your sub-sample of 400 observations
mydata <- dat[selected.rows,]
dim(mydata) #check the dimension of your sub-sample

#Calculate frequency and proportion of each category variable
Port.freq <- table(mydata$Port)
Port.prop <- round(prop.table(Port.freq) * 100, 1)
Protocol.freq <- table(mydata$Protocol)
Protocol.prop <- round(prop.table(Protocol.freq) * 100, 1)
Target.Honeypot.Server.OS.freq <- table(mydata$Target.Honeypot.Server.OS)
Target.Honeypot.Server.OS.prop <- round(prop.table(Target.Honeypot.Server.OS.freq) * 100, 1)
Source.OS.Detected.freq <- table(mydata$Source.OS.Detected)
Source.OS.Detected.prop <- round(prop.table(Source.OS.Detected.freq) * 100, 1)
Source.Port.Range.freq <- table(mydata$Source.Port.Range)
Source.Port.Range.prop <- round(prop.table(Source.Port.Range.freq) * 100, 1)
Source.IP.Type.Detected.freq <- table(mydata$Source.IP.Type.Detected)
Source.IP.Type.Detected.prop <- round(prop.table(Source.IP.Type.Detected.freq) * 100, 1)
APT.freq <- table(mydata$APT)
APT.prop <- round(prop.table(APT.freq) * 100, 1)

#Calculate missing count, missing percentage, min, max, mean, median and skewness of each continuous variable
Hits.missing.count <- as.numeric(sum(is.na(mydata$Hits)))
Hits.missing.percentage <- round((Hits.missing.count / length(mydata$Hits)) * 100, 1)
Hits.min <- round(min(mydata$Hits), 1)
Hits.max <- round(max(mydata$Hits), 1)
Hits.mean <- round(mean(mydata$Hits), 1)
Hits.median <- round(median(mydata$Hits), 1)
Hits.skewness <- round(skewness(mydata$Hits), 1)

Average.Request.Size.Bytes.missing.count <- as.numeric(sum(is.na(mydata$Average.Request.Size.Bytes)))
Average.Request.Size.Bytes.missing.percentage <- round((Average.Request.Size.Bytes.missing.count / length(mydata$Average.Request.Size.Bytes)) * 100, 1)
Average.Request.Size.Bytes.min <- round(min(mydata$Average.Request.Size.Bytes), 1)
Average.Request.Size.Bytes.max <- round(max(mydata$Average.Request.Size.Bytes), 1)
Average.Request.Size.Bytes.mean <- round(mean(mydata$Average.Request.Size.Bytes), 1)
Average.Request.Size.Bytes.median <- round(median(mydata$Average.Request.Size.Bytes), 1)
Average.Request.Size.Bytes.skewness <- round(skewness(mydata$Average.Request.Size.Bytes), 1)

Attack.Window.Seconds.missing.count <- as.numeric(sum(is.na(mydata$Attack.Window.Seconds)))
Attack.Window.Seconds.missing.percentage <- round((Attack.Window.Seconds.missing.count / length(mydata$Attack.Window.Seconds)) * 100, 1)
Attack.Window.Seconds.min <- round(min(mydata$Attack.Window.Seconds), 1)
Attack.Window.Seconds.max <- round(max(mydata$Attack.Window.Seconds), 1)
Attack.Window.Seconds.mean <- round(mean(mydata$Attack.Window.Seconds), 1)
Attack.Window.Seconds.median <- round(median(mydata$Attack.Window.Seconds), 1)
Attack.Window.Seconds.skewness <- round(skewness(mydata$Attack.Window.Seconds), 1)

Average.Attacker.Payload.Entropy.Bits.missing.count <- as.numeric(sum(is.na(mydata$Average.Attacker.Payload.Entropy.Bitss)))
Average.Attacker.Payload.Entropy.Bits.missing.percentage <- round((Average.Attacker.Payload.Entropy.Bits.missing.count / length(mydata$Average.Attacker.Payload.Entropy.Bits)) * 100, 1)
Average.Attacker.Payload.Entropy.Bits.min <- round(min(mydata$Average.Attacker.Payload.Entropy.Bits), 1)
Average.Attacker.Payload.Entropy.Bits.max <- round(max(mydata$Average.Attacker.Payload.Entropy.Bits), 1)
Average.Attacker.Payload.Entropy.Bits.mean <- round(mean(mydata$Average.Attacker.Payload.Entropy.Bits), 1)
Average.Attacker.Payload.Entropy.Bits.median <- round(median(mydata$Average.Attacker.Payload.Entropy.Bits), 1)
Average.Attacker.Payload.Entropy.Bits.skewness <- round(skewness(mydata$Average.Attacker.Payload.Entropy.Bits), 1)

Attack.Source.IP.Address.Count.missing.count <- as.numeric(sum(is.na(mydata$Attack.Source.IP.Address.Count)))
Attack.Source.IP.Address.Count.missing.percentage <- round((Attack.Source.IP.Address.Count.missing.count / length(mydata$Attack.Source.IP.Address.Count)) * 100, 1)
Attack.Source.IP.Address.Count.min <- round(min(mydata$Attack.Source.IP.Address.Count), 1)
Attack.Source.IP.Address.Count.max <- round(max(mydata$Attack.Source.IP.Address.Count), 1)
Attack.Source.IP.Address.Count.mean <- round(mean(mydata$Attack.Source.IP.Address.Count), 1)
Attack.Source.IP.Address.Count.median <- round(median(mydata$Attack.Source.IP.Address.Count), 1)
Attack.Source.IP.Address.Count.skewness <- round(skewness(mydata$Attack.Source.IP.Address.Count), 1)

Average.ping.to.attacking.IP.milliseconds.missing.count <- as.numeric(sum(is.na(mydata$Average.ping.to.attacking.IP.milliseconds)))
Average.ping.to.attacking.IP.milliseconds.missing.percentage <- round((Average.ping.to.attacking.IP.milliseconds.missing.count / length(mydata$Average.ping.to.attacking.IP.milliseconds)) * 100, 1)
Average.ping.to.attacking.IP.milliseconds.min <- round(min(mydata$Average.ping.to.attacking.IP.milliseconds), 1)
Average.ping.to.attacking.IP.milliseconds.max <- round(max(mydata$Average.ping.to.attacking.IP.milliseconds), 1)
Average.ping.to.attacking.IP.milliseconds.mean <- round(mean(mydata$Average.ping.to.attacking.IP.milliseconds), 1)
Average.ping.to.attacking.IP.milliseconds.median <- round(median(mydata$Average.ping.to.attacking.IP.milliseconds), 1)
Average.ping.to.attacking.IP.milliseconds.skewness <- round(skewness(mydata$Average.ping.to.attacking.IP.milliseconds), 1)

Average.ping.variability.missing.count <- as.numeric(sum(is.na(mydata$Average.ping.variability)))
Average.ping.variability.missing.percentage <- round((Average.ping.variability.missing.count / length(mydata$Average.ping.variability)) * 100, 1)
Average.ping.variability.min <- round(min(mydata$Average.ping.variability), 1)
Average.ping.variability.max <- round(max(mydata$Average.ping.variability), 1)
Average.ping.variability.mean <- round(mean(mydata$Average.ping.variability), 1)
Average.ping.variability.median <- round(median(mydata$Average.ping.variability), 1)
Average.ping.variability.skewness <- round(skewness(mydata$Average.ping.variability), 1)

Individual.URLs.requested.missing.count <- as.numeric(sum(is.na(mydata$Individual.URLs.requested)))
Individual.URLs.requested.missing.percentage <- round((Individual.URLs.requested.missing.count / length(mydata$Individual.URLs.requested)) * 100, 1)
Individual.URLs.requested.min <- round(min(mydata$Individual.URLs.requested), 1)
Individual.URLs.requested.max <- round(max(mydata$Individual.URLs.requested), 1)
Individual.URLs.requested.mean <- round(mean(mydata$Individual.URLs.requested), 1)
Individual.URLs.requested.median <- round(median(mydata$Individual.URLs.requested), 1)
Individual.URLs.requested.skewness <- round(skewness(mydata$Individual.URLs.requested), 1)

IP.Range.Trust.Score.missing.count <- as.numeric(sum(is.na(mydata$IP.Range.Trust.Score)))
IP.Range.Trust.Score.missing.percentage <- round((IP.Range.Trust.Score.missing.count / length(mydata$IP.Range.Trust.Score)) * 100, 1)
IP.Range.Trust.Score.min <- round(min(mydata$IP.Range.Trust.Score), 1)
IP.Range.Trust.Score.max <- round(max(mydata$IP.Range.Trust.Score), 1)
IP.Range.Trust.Score.mean <- round(mean(mydata$IP.Range.Trust.Score), 1)
IP.Range.Trust.Score.median <- round(median(mydata$IP.Range.Trust.Score), 1)
IP.Range.Trust.Score.skewness <- round(skewness(mydata$IP.Range.Trust.Score), 1)


#Plotting histogram for continuous variables
ggplot(mydata,aes(x=Hits)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Average.Request.Size.Bytes)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Attack.Window.Seconds)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Average.Attacker.Payload.Entropy.Bits)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Attack.Source.IP.Address.Count)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Average.ping.to.attacking.IP.milliseconds)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Average.ping.variability)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=Individual.URLs.requested)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")

ggplot(mydata,aes(x=IP.Range.Trust.Score)) +
  geom_histogram() + #Histogram with default settings
  ylab("Frequency")


# **********************************************************

# Define variables to exclude
# exclude_vars <- c(
#   "dat", 
#   "mydata", 
#   "freq.var", 
#   "categorical.vars", 
#   "item", 
#   "max.var", 
#   "min.var", 
#   "median.var", 
#   "mean.var",
#   "skewness.var",
#   "missing.count", 
#   "missing.proportion", 
#   "missing.var",
#   "numeric.vars",
#   "selected.rows",
#   "total")
# 
# # Get all objects (variables) in the environment, excluding specific ones
# env_vars <- setdiff(ls(), exclude_vars)
# 
# # Create an empty list to store variable names and values
# env_list <- list()
# 
# # Loop over each variable and store its value
# for (var in env_vars) {
#   env_list[[var]] <- get(var)  # Retrieve the value of the variable
# }
# 
# # Convert the list to a data frame
# env_df <- data.frame(
#   Variable = names(env_list),
#   Value = sapply(env_list, function(x) paste(x, collapse=", "))  # Convert values to text
# )
# 
# # Export to CSV
# write.csv(env_df, "R_Environment_Variables_02.csv", row.names=FALSE)