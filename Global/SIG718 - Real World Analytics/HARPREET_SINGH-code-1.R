################################# 

#################################

#############################################################################################

#############################################################################################
library(ggplot2)
##################################
#Question 1 - Understand the Data
##################################

the.data <- as.matrix(read.table("ENB_2023.txt"))  

set.seed(123456) # using your student ID number for reproducible sampling with the seed function

# Subsetting the data
my.data <- the.data[sample(1:671,340),c(1:6)] 
df <- my.data

# Checking top 5 rows of data
head(df)

#finding the correlation between the variables
df <- data.frame(df)
round(cor(df), 2)

# Use scatter plots and histograms to understand the relationship between each of the 
# variables X1, X2, X3, X4, X5, and your variable of interest Y.

# Create 5 scatterplots function (for each X variable against the variable of interest Y) 
ggplot(df,  aes(x = V1, y = V6)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "X1 vs Y", x = "X1: Temperature in kitchen area, in Celsius", y = "Y: Appliances, energy use, in Wh")

ggplot(df,  aes(x = V2, y = V6)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "X2 vs Y", x = "X2: Humidity in kitchen area, given as a percentage", y = "Y: Appliances, energy use, in Wh")

ggplot(df,  aes(x = V3, y = V6)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "X3 vs Y", x = "X3: Temperature outside (from weather station), in Celsius", y = "Y: Appliances, energy use, in Wh")

ggplot(df,  aes(x = V4, y = V6)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "X4 vs Y", x = "X4: Humidity outside (from weather station), given as a percentage", y = "Y: Appliances, energy use, in Wh")

ggplot(df,  aes(x = V5, y = V6)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "X5 vs Y", x = "X5: Visibility (from weather station), in km", y = "Y: Appliances, energy use, in Wh")


# Create 6 histograms for each X variable and Y
# Function to plot a histogram along with a few additional details about the distribution. 

##install.packages('moments')
#install.packages('dplyr')
library(moments)
library(dplyr)
# Create a dataframe to store the skewness and its interpretation per variable

# Define a list of variable names
variables <- rbind("V1", "V2", "V3", "V4","V5","V6")

# Store the skewness in order of variable names defined above
skewness_vals  <- rbind(round(skewness(df$"V1"),2), 
                        round(skewness(df$"V2"),2), 
                        round(skewness(df$"V3"),2),
                        round(skewness(df$"V4"),2),
                        round(skewness(df$"V5"),2),
                        round(skewness(df$"V6"),2))

# Interpret the skewness per variable according to the values stored above
inferences <- c()
for(skew_val in skewness_vals){
  if(between(skew_val, -0.5, 0.5)){
    i <-  "Fairly Symmetrical"
  }
  else if(between(skew_val, -1, -0.5) ){
    i <- "Moderately left skewed"
  } 
  else if (between(skew_val, 0.5, 1)){
    i <- "Moderately right skewed"
  }
  else if((skew_val < -1 )| (skew_val > 1)){
    i <-  "Highly Skewed"
  }
  inferences <- append(inferences, i)
}  
# Combine the variable names, skewness values, and their corresponding inferences in one dataframe and display it.
skew_df <- data.frame(variables, skewness_vals, inferences)

skew_df

create_histogram <- function(x, xlab="", pos="topright", rounding=2){
  mean <- round(mean(x), rounding)
  meanlabel <- paste("Mean   (", mean, ")")
  median <- round(median(x), rounding)
  medianlabel <- paste("Median (", median, ")")
  min <- round(min(x), rounding)
  minlabel <- paste("Min    (", min, ")")
  max <- round(max(x),rounding)
  maxlabel <- paste("Max    (", max, ")")
  
  title = paste("Histogram of ", xlab)
  
  hist(x, xlab=xlab, main=title)
  abline(v = mean, col = 2, lwd=3)
  abline(v = median, col = 7, lwd=3)
  abline(v = min, col = 4, lwd=3)
  abline(v = max, col = 4, lwd=3)
  legend(x = pos,                    # Position 
         cex = 0.75,                 # Size of text
         legend = c(meanlabel, medianlabel, minlabel, maxlabel),  # Legend texts
         lty = c(1),                 # Line types
         col = c(2, 7, 4, 4),        # Line colors
         lwd = 3)                    # Line width
}
#Create Histogram for RM:
create_histogram(df$V1, "Temperature in kitchen area, in Celsius", "topright") 

#Create Histogram for RM:
create_histogram(df$V2, "Humidity in kitchen area", "topright") 

#Create Histogram for RM:
create_histogram(df$V3, "Temperature outside (from weather station), in Celsius", "topright") 

#Create Histogram for RM:
create_histogram(df$V4, "Humidity outside (from weather station), given as a percentage", "topright") 

#Create Histogram for RM:
create_histogram(df$V5, "Visibility (from weather station), in km", "topright") 

#Create Histogram for RM:
create_histogram(df$V6, "Appliances, energy use, in Wh", "topleft") 



################################
#Question 2 - Transform the Data
################################

# Step 1: Store  the variables to be transformed in a new dataframe, so as to not affect the original dataset
df = subset(df, select = -c(V5) )
head(df)
variables_to_transform <- df[]
variables_to_transform
# Let us take a look at the number of rows and columns in this selection
message("columns = ", ncol(variables_to_transform), "; rows = ", nrow(variables_to_transform))


# Step 2: create a table of same dimension as the variables_to_transform and initialize it with zero
data.transformed <- matrix(0, nrow(variables_to_transform), ncol(variables_to_transform))

data.transformed
# Defining simple data transformation functions:


  # Min-Max Transformation
min_max_transform <- function(x, input_data=x){
    min = min(input_data)
    max = max(input_data)
    mm_x = (x - min)/(max - min)
  }

# Applying data transformation function to the required columns:
# Col #1: V1, Col #2 : V2, Col #3: V3, Col #4: V4, Col #5: V5, Col #6: V6

data.transformed[,1]=min_max_transform(df[,1])
data.transformed[,2]=min_max_transform(df[,2])
data.transformed[,3]=min_max_transform(df[,3])
data.transformed[,4]=min_max_transform(df[,4])
data.transformed[,5]=min_max_transform(df[,5])
#data.transformed[,6]=min_max_transform(df[,6])


# Viewing a sample of the transformed dataset
head(data.transformed)


summary(data.frame(data.transformed))


# Save this transformed data to a text file
write.table(data.transformed, "HARPREET_SINGH-transformed.txt")  # replace “name” with either your surname or first name.



##########################################
#Question 3 T3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("HARPREET_SINGH-transformed.txt"))  # import your saved data
head(data.transformed_copy)
# Get weights for Weighted Arithmetic Mean with fit.QAM() 

fit.QAM(data.transformed_copy, output.1="AM-output.txt",stats.1="AM-stats.txt", g=AM, g.inv=invAM) # by default, it uses AM


# Get weights for Power Mean p=0.5 and p=2 with fit.QAM()
fit.QAM(data.transformed_copy,output.1="PM05output1.txt",stats.1="PM05stats1.txt", g=PM05,g.inv = invPM05) # p = 0.5

#p = 2, g= QM, g.inv = invQM
fit.QAM(data.transformed_copy,output.1="QMoutput1.txt",stats.1="QMstats1.txt",g=QM,g.inv = invQM) # p = 2

# Get weights for Ordered Weighted Average with fit.OWA()

fit.OWA(data.transformed_copy,"OWAoutput1.txt","OWAstats1.txt") # OWA


# Get weights for Choquet Integral with fit.choquet() - Optional

fit.choquet(data.transformed_copy,"choquetoutput1.txt","choquetstats1.txt") # OWA



#######################################
#Question 4 T4.- Use Model for Prediction T4.
#######################################

# new_input has X1=22; X2=38; X3=4; X4=88.2; X5=34


df_test <- data.frame(X1 = c(22),
                      X2 = c(38),
                      X3 = c(4),
                      X4 = c(88)
)
print(df_test)

head(df)
min_1 = min(df[,1])
max_1 = max(df[,1])
min_2 = min(df[,2])
max_2 = max(df[,2])
min_3 = min(df[,3])
max_3 = max(df[,3])
min_4 = min(df[,4])
max_4 = max(df[,4])
min_5 = min(df[,5])
max_5 = max(df[,5])
# Applying data transformation function to the required columns:
# Col #1: V1, Col #2 : V2, Col #3: V3, Col #4: V4, Col #5: V5, Col #6: V6


# Step 2: create a table of same dimension as the variables_to_transform and initialize it with zero
data.transformed.test <- matrix(0, nrow(df_test), ncol(df_test))

data.transformed.test

# Defining simple data transformation functions:--transforming the test data
data.transformed.test[,1]=(df_test[,1] - min_1) / (max_1 - min_1)
data.transformed.test[,2]=(df_test[,2] - min_2) / (max_2 - min_2)
data.transformed.test[,3]=(df_test[,3] - min_3) / (max_3 - min_3)
data.transformed.test[,4]=(df_test[,4] - min_4) / (max_4 - min_4)


data.transformed.test


# Let us take a look at the number of rows and columns in this selection
message("columns = ", ncol(data.transformed.test), "; rows = ", nrow(data.transformed.test))

install.packages("Rfmtool")
##weights of best fitting function
w = c(0.811890990424478, 0.0766653925840138, 0.111443616991504, 0) 
w

# applying the transformed variables to the best model selected from Q3 for Y prediction
data.transformed.test
y_pred <- OWA(data.transformed.test, w)
y_pred



# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer


# Compare your prediction with the measured value of Y, Y=100.
y_pred_rescale = (y_pred * (max_5 - min_5)) + min_5
y_pred_rescale

