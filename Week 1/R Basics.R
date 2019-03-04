# R basics ----

sqrt(2) #square root function
abs(-24) # absolute value 

?dim # seeking help with functions in R

HoursYear = 24*365 # assigning variable
HoursYear # print variable in console

ls() # list all variables in current session


# Vectors and data frames ----
c(2,3,5,8,13) # create a vector using the combine function
c(1:100) # alternate way to assign sequential numbers to a vector
seq(0,100,2) # Another way to create a vector with numbers from 0 to 100 in a increments of 2
Country = c("Brazil","China","India","Switzerland","USA")
LifeExpectancy = c(74,76,65,83,79)
Country[1] # Accessing a value of the vector using [] 

CountryData = data.frame(Country,LifeExpectancy) # Create data frame by combining two vectors
CountryData

CountryData$Population = c(199000,1390000,1240000,7997,318000) # Add a new variable to the data frame 
CountryData

Country = c("Australia","Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewCountryData = data.frame(Country,LifeExpectancy,Population)

AllCountryData = rbind(CountryData, NewCountryData) # Combining two data frames using rbind function
AllCountryData

# Working with files ----
# Reference to file used: "WHO.csv"

getwd() # Get working directory
setwd("~/MIT Analytics Edge/Week 1") # Set working directory to the folder where the file to be loaded is

WHO = read.csv("WHO.csv") # Save the data in csv as a data frame 
WHO

str(WHO) # Display structure of data frame
summary(WHO) # Display summary of data frame 

WHO_Europe = subset(WHO, Region == "Europe") # Subset a data frame by using subset() function
WHO_Europe
summary(WHO_Europe)

write.csv(WHO_Europe, "WHO_Europe.csv") # Write to csv using the write.csv() function

rm(WHO_Europe) # Remove a data structure no longer required

# Working with data in data frames ----

WHO$Under15 # Access a variable in the data frame using the $ notation
mean(WHO$Under15) # Mean function returns the mean value 
sd(WHO$Under15) # SD function returns std deviation 
summary(WHO$Under15) # Summary function can provide summary data for a specified variable 

which.min(WHO$Under15) # which.min function returns the minimum value in the dataset
WHO$Country[86]

which.max(WHO$Under15) # which.max function returns the maximum value in the dataset
WHO$Country[124]

# Plots ----

plot(WHO$GNI, WHO$FertilityRate) # Creates a scatterlpot with first variable in x-axis and second one in y-axis

Outliers = subset(WHO, GNI>10000 & FertilityRate > 2.5 ) # Subset function with two conditions separated by & 
Outliers

nrow(Outliers) # Show number of rows in dataset

Outliers[c("Country","GNI","FertilityRate")]

# Other plots and summary tables ----

hist(WHO$CellularSubscribers) # Create a histogram (shows distribution of variable)
boxplot(WHO$LifeExpectancy ~ WHO$Region) # Create a boxplot for first variable grouped by second variable -- shows statistical range of a variable
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab = "", ylab = "Life Expectancy", main = "Life Expectancy of Countries by Region") # Box plot with labels

table(WHO$Region) # Summary table; works well for variables with less values

tapply(WHO$Over60,WHO$Region,mean) # tapply function -- shows the third arg function of first arg grouped by the second arg
tapply(WHO$LiteracyRate,WHO$Region,min, na.rm=TRUE) # na.rm=TRUE removes the NA values from the dataset before computing to bring the summary results

