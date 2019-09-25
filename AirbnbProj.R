#Setting up the working directory according to me
#install package dplyr
#we are using the dplyr because we need to filter the data according to our need
install.packages("dplyr")

#getting library for dplyr
library(dplyr)

#Question 1

#What is the mean, median and standard deviation of the number of reviews per property in your assigned city?
#Loading venice data from a csv file into a new variable called Venice_Data
Venice_Data <- read.csv("Venice.csv")

#printing the loaded data in Venice_Data variable
print(Venice_Data)

#Mean of the number of reviews per property in Venice
Mean_Reviews_Per_Listing_Venice <- mean(Venice_Data$number_of_reviews)

#Median of the number of reviews per property in Venice
Median_Reviews_Per_Listing_Venice <- median(Venice_Data$number_of_reviews)

#Standard deviation of the number of reviews per property in Venice
SD_Reviews_Per_Listing_Venice <- sd(Venice_Data$number_of_reviews)

#Display Mean,Median and Mode of reviews per property

print(paste("The Mean of the number of reviews per property is",Mean_Reviews_Per_Listing_Venice))

print(paste("The Median of the number of reviews per property is",Median_Reviews_Per_Listing_Venice))

print(paste("The Standard deviation of the number of reviews per property is",SD_Reviews_Per_Listing_Venice))

#Question 2

#What is the mean price per night for a Private Room in your assigned city?
#mean price per night for a Private Room in Venice
#assigning the average price per night for a private room in Venice to Mean_Privateroom_Venice

Mean_Privateroom_Venice <- Venice_Data %>%
  select(price) %>%
  filter(Venice_Data$room_type == "Private room") %>%
  summarise(mean_price = mean(price))

#printing the average price we get in the variable Mean_Privateroom_Venice
print(Mean_Privateroom_Venice)

#Question 3

#How much above the average price would it cost to stay for a night in the most expensive private room in the city?
#assigning the max value for a night in a private room in Venice to Max_Privateroom_Venice

Max_Privateroom_Venice <- Venice_Data %>%
  select(price) %>%
  filter(Venice_Data$room_type == "Private room") %>%
  summarise(Most_Expensive_Room = max(price))

#printing the max value we get in the variable Max_Privateroom_Venice
print(Max_Privateroom_Venice)

#Difference between average price and the most expensive private room in Venice
#Assigning a new variable to hold the difference between of average price and most expensive price of a private room in Venice

Difference_Venice = Max_Privateroom_Venice - Mean_Privateroom_Venice

#changing the name of coloumn header to Difference
names(Difference_Venice) = c("Difference")

#printing the difference between of average price and most expensive price of a private room in Venice
print(Difference_Venice)

#Question 4

#How do the prices in Q2 & Q3 compare to Dublin?

#Loading dublin data in a from a csv file into a new variable called Dublin_Data
Data_Dublin <- read.csv("Dublin.csv")

#printing the loaded data in Dublin_Data variable
print(Data_Dublin)

#assigning the average price per night for a private room in Dublin to Mean_Privateroom_Dublin 
Mean_Privateroom_Dublin <- Data_Dublin %>%
  select(price) %>%
  filter(Data_Dublin$room_type == "Private room") %>%
  summarise(Average_Price = mean(price))

#printing the average price we get in the variable Mean_Privateroom_Dublin
print(Mean_Privateroom_Dublin)

#average price for a night in the different room category to cross check the above value
tapply(Data_Dublin$price, INDEX = Data_Dublin$room_type, FUN = mean)

#assigning the max value for a night in a private room in Venice to Max_Privateroom_Dublin 
Max_Privateroom_Dublin <- Data_Dublin %>%
  select(price) %>%
  filter(Data_Dublin$room_type == "Private room") %>%
  summarise(Most_Expensive_Room = max(price))

#printing the max value we get in the variable Max_Privateroom_Dublin
print(Max_Privateroom_Dublin)

#most expensive price for a night in the different room category to cross check the above value
tapply(Data_Dublin$price, INDEX = Data_Dublin$room_type, FUN = max)

#difference between average price and the most expensive private room in Dublin
#assigning a new variable to hold the difference between of average price and most expensive price of a private room in Dublin
Difference_Dublin <- c(Max_Privateroom_Dublin - Mean_Privateroom_Dublin)

#changing the name of coloumn header to Difference
names(Difference_Dublin) = c("Difference")

#printing the difference between of average price and most expensive price of a private room in Dublin
print(Difference_Dublin)

#Question 5

#What is the name and ID of the host with the most properties listed on Airbnb for your assigned city?

# Finding the Row number for Maximum host listings in the column 15, Calculated_Host_Listings_Count

max_listing_Row_Num<-which.max(Venice_Data[,15])

#Finding the name of Host who has max listings with the help of above data

name<-Venice_Data[max_listing_Row_Num,4]

# Finding the Host_id for the host with max listings

host_id<-Venice_Data[max_listing_Row_Num,3]

print(paste("The host with the most properties on AirBnB in Venice is",name))

print(paste("The host_id for the host with the most properties on AirBnB in Venice is",host_id))

# Finding the Row number for Maximum host listings in the column 15, Calculated_Host_Listings_Count

max_listing_Row_Num<-which.max(Venice_Data[,15])

#Finding the name of Host who has max listings with the help of above data

name<-Venice_Data[max_listing_Row_Num,4]

#Increasing the printing limit in console

options(max.print=999999)

# Finding the Host_id for the host with max listings

max_listing_host_id<-Venice_Data[max_listing_Row_Num,3]

#Making a new data frame with only the data for the host with the max listings

max_host_data_subset <- subset(Venice_Data,host_id==max_listing_host_id)

#Question 6

#How much could this host potentially earn per week from their Airbnb listings?

#Calculating the Weekly earning per listing for the host with max listings considering Only those Days in a year for which
#the property is available and minimum nights for which the property can be rented

Number_of_minimum_nights_factors=floor(max_host_data_subset$availability_365/max_host_data_subset$minimum_nights)

Weekly_Earning_per_listing=Number_of_minimum_nights_factors*max_host_data_subset$minimum_nights*max_host_data_subset$price/52

#Adding a new column to the Data frame, for host with max listings, the new data will have a new column in last as Weekly_Earning_per_listing

max_host_data_with_Weekly_Earning=cbind(max_host_data_subset,Weekly_Earning_per_listing)

#Calculating the Sum of weekly earnings from all listings and rounding it

Total_Weekly_Earning=round(sum(max_host_data_with_Weekly_Earning[,17]),3)

#Question 7

#Does this host have any properties listed in Dublin? If yes, how many?

#Importing the data for Dublin, to find the number of listings for the same host who has max listings in Venice

Data_Dublin <- read.csv("Dublin.csv")

#Finding the number of Listings in Dublin for the Loop

j=nrow(Data_Dublin)

m=1
d1=0

# Finding the number of matching host_id (calculated in d1),for listings in Venice, which are also present in Dublin for the host with maximum listings

while(m<=j){
  if (Data_Dublin[m,3]==max_listing_host_id)
    d1=d1+1
  m=m+1
} 

print(paste("The host with the most properties on AirBnB in Venice is",name,",","his host_id for AirBnB is", max_listing_host_id,".","His weekly earnings, taking into consideration Only those Days in a year for which the property is available and minimum nights for which the property can be rented,are US$",Total_Weekly_Earning))

print(paste("The number of Listings, for the same host in Dublin who also has the maximum listings in Venice, are",d1))

#Question 8

# Write a function (called “mostHomes”) to determine the which neighbourhood in a city has the most entire homes/apartments listed on Airbnb and use this function to answer that question for your assigned city and Dublin?
# create the function mostHomes

mostHomes <- function(M){
  N <- subset(M, M$room_type == "Entire home/apt")
  x <- table(N$neighbourhood)
  y <- as.character(names(x)[which.max(x)])
  return(y)
}

#call the function for both cities
mostHomes(Data_Dublin)
mostHomes(Venice_Data)
