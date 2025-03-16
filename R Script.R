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
View(mydata)
#Create a new vector containing all the categorical/binary variables
categorical.vars <- c(
  "Port", 
  "Protocol", 
  "Target.Honeypot.Server.OS", 
  "Source.OS.Detected",
  "Source.Port.Range", 
  "Source.IP.Type.Detected", "APT"
)

#Loop over the newly created vector
for (item in categorical.vars)
{
  # A dynamic variable based on name of category variable, i.e Port.freq
  freq.var <- paste0(item,".freq")

  # Get frequency table of each category and assign it to the variable
  assign(freq.var, table(mydata[[item]]))

  # Get proportions of each frequency table and assign it to a new variable, i.e Port.prop
  assign(paste0(item,".prop"), round(prop.table(get(freq.var)) * 100, 1))
}

#Create a new vector containing all continuous/numeric variables
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
##########################
#Loop over the newly created vector
for (item in numeric.vars)
{
  # Dynamic variables based on name of continuous variables, i.e Hits.min
  min.var <- paste0(item,".min")
  max.var <- paste0(item,".max")
  mean.var <- paste0(item,".mean")
  median.var <- paste0(item,".median")
  skewness.var <- paste0(item,".skewness")
  missing.count.var <- paste0(item, ".missing.count")
  missing.percentage.var <- paste0(item, ".missing.percentage")
  
  total <- length(mydata[[item]])  # Length of the sub-sample
  missing.count <- as.numeric(sum(is.na(mydata[[item]])))  # Count of missing values
  missing.proportion <- round((missing.count / total) * 100, 1)  # Convert count to percentage
  
  # 
  assign(missing.count.var, missing.count)
  assign(missing.percentage.var, missing.proportion)
  assign(min.var, round(min(mydata[[item]]) * 100, 1))
  assign(max.var, round(max(mydata[[item]]) * 100, 1))
  assign(mean.var, round(mean(mydata[[item]]) * 100, 1))
  assign(median.var, round(median(mydata[[item]]) * 100, 1))
  assign(skewness.var, round(skewness(mydata[[item]]) * 100, 1))
}
# **********************************************************

# Define variables to exclude
exclude_vars <- c(
  "dat", 
  "mydata", 
  "freq.var", 
  "categorical.vars", 
  "item", 
  "max.var", 
  "min.var", 
  "median.var", 
  "mean.var",
  "skewness.var",
  "missing.count", 
  "missing.proportion", 
  "missing.var",
  "numeric.vars",
  "selected.rows",
  "total")

# Get all objects (variables) in the environment, excluding specific ones
env_vars <- setdiff(ls(), exclude_vars)

# Create an empty list to store variable names and values
env_list <- list()

# Loop over each variable and store its value
for (var in env_vars) {
  env_list[[var]] <- get(var)  # Retrieve the value of the variable
}

# Convert the list to a data frame
env_df <- data.frame(
  Variable = names(env_list),
  Value = sapply(env_list, function(x) paste(x, collapse=", "))  # Convert values to text
)

# Export to CSV
write.csv(env_df, "R_Environment_Variables.csv", row.names=FALSE)