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

# Create a new vector containing all the categorical/binary variables
categorical.vars <- c(
  "Port", 
  "Protocol", 
  "Target.Honeypot.Server.OS", 
  "Source.OS.Detected",
  "Source.Port.Range", 
  "Source.IP.Type.Detected",
  "APT"
)

# Create empty lists to store frequency and proportion tables
freq.list <- list()
prop.list <- list()

# Iterate over the newly created vector and calculate frequency & proportion table for each variable
for (var in categorical.vars) {
  freq.list[[var]] <- table(mydata[[var]]);  # Get frequency table
  prop.list[[var]] <- round(prop.table(freq.list[[var]]) * 100, 1)  # Get proportion table
  
}
freq.list; # Print the list containing all the frequency tables
prop.list  # Print the list containing all the proportion tables

# Create a new vector containing all the continuous/numeric variables
numeric.vars <- c(
  "Hits",
  "Average.Request.Size.Bytes",
  "Attack.Window.Seconds",
  "Average.Attacker.Payload.Entropy.Bits",
  "Attack.Source.IP.Address.Count",
  "Average.ping.to.attacking.IP.milliseconds",
  "Average.ping.variability",
  "Individual.URLs.requested",
  "IP.Range.Trust.Score"
)

# Create empty lists to store statistics including min, max, etc.
missing.count.list <- list()
missing.percent.list <- list()
min.list <- list()
max.list <- list()
mean.list <- list()
median.list <- list()
skewness.list <- list()

for (var in numeric.vars) {
  missing.count.list[[var]] <- sum(is.na(mydata[[var]]))
  missing.percent.list[[var]] <- round((missing.count.list[[var]] / length(mydata[[var]])) * 100, 1)
  min.list[[var]] <- round(min(mydata[[var]]), 1)
  max.list[[var]] <- round(max(mydata[[var]]), 1)
  mean.list[[var]] <- round(mean(mydata[[var]]), 1)
  median.list[[var]] <- round(median(mydata[[var]]), 1)
  skewness.list[[var]] <- round(skewness(mydata[[var]]), 1)
}

missing.count.list
missing.percent.list
min.list
max.list
mean.list
median.list
skewness.list