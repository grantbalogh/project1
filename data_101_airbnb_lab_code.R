#installing packages used for data wrangling in this lab using install.packages() and library() functions
install.packages('ggplot2')
install.packages('dplyr')
library(ggplot2)
library(dplyr)

#Step 1: checking my working dictionary within DA_101 folder
getwd()

#Step 3: creating a dictionary to include (host_name, host_neighbourhood, zipcode, latitude, longitude) using the list() function
#this dictionary is named Most_Expensive_Airbnb
Most_Expensive_Airbnb <- list('host_name'= "Vladimir", 'host_neighbourhood'= "Fenway/Kenmore", 'zipcode'= 2215, 'latitude' = 42.3479345, 'longitude' = -71.09757989)

#step 4: Creating a data frame, to store data, with the 5 columns from above and reviewing 10 most expensive AirBnb data 
#created a list for each of the information needed
#lists are named after the 5 columns above
host_name <- c('Vladimir', 'Robert', '滢', 'Elisa', 'Mario', 'Lisa', 'Lakshimi', 'Erica', 'Nastia', 'Mario')
host_neighbourhood <- c('Fenway/Kenmore', 'South Boston', 'Roxbury', 'Fenway/Kenmore', 'Beacon Hill', 'South End', 'Fenway/Kenmore', 'Back Bay', 'South Boston', 'Beacon Hill' ) 
#host_neighbourhood not available for the 2 Myrtle Street locations, used the neighbourhood_cleansed data for them
zipcode <- c(2215, 2210, 2118, 2115, 2114, 2118, 2115, 2116, 2210, 2114)
latitude <- c(42.3479345, 42.34835843, 42.3386258, 42.34520065, 42.35836337, 42.34241483, 42.34185766, 42.34997061, 42.35242659, 42.35800212)
longitude <- c(-71.09757989, -71.0398751, -71.0833152, -71.08891765, -71.06840785, -71.07451934, -71.08904437, -71.08103177, -71.04256214, -71.0689558)
#creating data frame of the top ten most expensive listings using the list values using the data.frame() function
#data frame is named Most_Expensive_Airbnb_Top_10_df, I think it is self explanatory 
Most_Expensive_Airbnb_Top_10_df <-data.frame(host_name, host_neighbourhood, zipcode, latitude, longitude)
#retrieving the mean of the latitude using mean() function
mean(latitude)
#retrieving the mean of the longitude using mean() function 
mean(longitude)

#Step 5: opening and importing 3 data sets (listings, calendar,reviews) using the read.csv() function to create data frames
#these data sets were provided and were named as ordered
listings_df <- read.csv("listings.csv")
calendar_df <- read.csv("calendar.csv") 
reviews_df <- read.csv("reviews.csv") 
#using dim() function to note the observations and columns in each data frame, basically checking the dimensions
dim(listings_df)
dim(calendar_df)
dim(reviews_df) 

#Step 6: finding the unique listing data in the host_neighbourhood column using unique() function
#extracted host_neighbourhood column from listings data frame by creating a vector
#used the requested host_neighbourhood column and attached _vector to indicate that it is a vector
host_neighbourhood_vector <- listings_df['host_neighbourhood']
#using the unique() function to find the unique values
#created a data frame named host_neighbourhood_unique in order to use the count() function easier 
host_neighbourhood_unique <- unique(host_neighbourhood_vector)
#using the count() function to count the unique values
#...it includes the blank values so we can assume it is n-1 [in this case 54-1 = 53 unique values]
count(host_neighbourhood_unique)

#Step 7: Selecting the 11 variables from listing_df that I want to analyze using the select() function
#the select() function will allow me to draw out the columns I am interested in
#ListingData data frame is named after the listings_df and a select few columns... seemed most concise to me and makes sense to me
ListingsData_df <- select(listings_df, host_response_rate, host_acceptance_rate, price, weekly_price, monthly_price, security_deposit, cleaning_fee, guests_included, number_of_reviews, review_scores_rating, reviews_per_month)
#Providing a descriptive table that shows min.,max.,mean,median,variance and std. dev. by inputting the data derived from the summary(),var(), and sd() functions
#using summary() function on ListingsData_df to give me min,max,median, and mean 
#summary() function is key to understanding and summarizing all data
summary(ListingsData_df)
#using var() on ListingsData_df on each variable to give me variance, using na.rm to report data with no missing values
#used $ operator to more easily pull data from the data frame in question
var(ListingsData_df$host_response_rate, na.rm=TRUE)
var(ListingsData_df$host_acceptance_rate, na.rm=TRUE)
var(ListingsData_df$price, na.rm=TRUE)
var(ListingsData_df$weekly_price, na.rm=TRUE)
var(ListingsData_df$monthly_price, na.rm=TRUE)
var(ListingsData_df$security_deposit, na.rm=TRUE)
var(ListingsData_df$cleaning_fee, na.rm=TRUE)
var(ListingsData_df$guests_included, na.rm=TRUE)
var(ListingsData_df$number_of_reviews, na.rm=TRUE)
var(ListingsData_df$review_scores_rating, na.rm=TRUE)
var(ListingsData_df$reviews_per_month, na.rm=TRUE)
#using sd() function on ListingsData_df on each variable to give me standard deviation, using na.rm to report data with no missing values
#used $ operator to more easily pull data from the data frame in question
sd(ListingsData_df$host_response_rate, na.rm=TRUE)
sd(ListingsData_df$host_acceptance_rate, na.rm=TRUE)
sd(ListingsData_df$price, na.rm=TRUE)
sd(ListingsData_df$weekly_price, na.rm=TRUE)
sd(ListingsData_df$monthly_price, na.rm=TRUE)
sd(ListingsData_df$security_deposit, na.rm=TRUE)
sd(ListingsData_df$cleaning_fee, na.rm=TRUE)
sd(ListingsData_df$guests_included, na.rm=TRUE)
sd(ListingsData_df$number_of_reviews, na.rm=TRUE)
sd(ListingsData_df$review_scores_rating, na.rm=TRUE)
sd(ListingsData_df$reviews_per_month, na.rm=TRUE)

#Step 8: create 5 new data sets for 5 different neighborhoods
#creating data set for subset by using select() function on listing_df
#looking at 3 variables:host_neighbourhood, price, review_scores_rating
#Listings_neighborhood_Data data frame is named after the listings_df and a select few columns
Listings_neighborhood_Data <- select(listings_df, host_neighbourhood, price, review_scores_rating)
#used host_neighbourhood_vector to view and select the neighborhoods I am interested in
#named data frame based off format provided host_neighborhood_df
#creating subsets using the filter() function on each host_neighborhood
#the filter() function will allow me to filter rows I am interested in 
Roxbury_df <- filter(Listings_neighborhood_Data, host_neighbourhood == 'Roxbury')
South_End_df <- filter(Listings_neighborhood_Data, host_neighbourhood == 'South End')
Fenway_Kenmore_df <- filter(Listings_neighborhood_Data, host_neighbourhood == 'Fenway/Kenmore')
Mattapan_df <- filter(Listings_neighborhood_Data, host_neighbourhood == 'Mattapan')
East_Boston_df <- filter(Listings_neighborhood_Data, host_neighbourhood == 'East Boston')
#using dim() function to report dimensions and note observations for each neighborhood selected above
dim(Roxbury_df)
dim(South_End_df)
dim(Fenway_Kenmore_df)
dim(Mattapan_df)
dim(East_Boston_df) 

#Step 9: Analysis of price and review scores rating using summarise() function
#summarise() function used to summarise information and data 
#used the pipe operator %>% for chaining functions and improve the readability of the code
#Roxbury analysis of average price and average review scores rating using the summarise() function, using na.rm to report data with no missing values
Roxbury_df %>% 
  summarise(avg_price = mean(price))
mean(Roxbury_df$review_scores_rating, na.rm=TRUE)
#South End analysis of average price and average review scores rating using the summarise() function, using na.rm to report data with no missing values
South_End_df %>% 
  summarise(avg_price = mean(price))
mean(South_End_df$review_scores_rating, na.rm=TRUE)
#Fenway-Kenmore analysis of average price and average review scores rating using the summarise() function, using na.rm to report data with no missing values
Fenway_Kenmore_df %>% 
  summarise(avg_price = mean(price))
mean(Fenway_Kenmore_df$review_scores_rating, na.rm=TRUE)
#Mattapan analysis of average price and average review scores rating using the summarise() function, using na.rm to report data with no missing values
Mattapan_df %>% 
  summarise(avg_price = mean(price))
mean(Mattapan_df$review_scores_rating, na.rm=TRUE)
#East Boston analysis of average price and average review scores rating using the summarise() function, using na.rm to report data with no missing values
East_Boston_df %>% 
  summarise(avg_price = mean(price))
mean(East_Boston_df$review_scores_rating, na.rm=TRUE)

#Step 10: Data visualization 
#Create 3 visualizations looking at the relationship between prices and review scores rating of listings
#extracting price and review_scores_rating from listings_df and creating vectors 
#named vectors after the columns I created them from
#stumbled into an error using [] single brackets, so I replaced them with [[]] 
#used [[]] double brackets to extract price and review_scores_rating in listings data frame
Price_Vector = listings_df[["price"]]
Review_scores_rating_vector = listings_df[["review_scores_rating"]]
#using ggplot() function to create my plots, running the code provided
ggplot(listings_df, aes(x=Review_scores_rating_vector, y=Price_Vector))
#per the textbook adding geom_point() creates a scatter plot by introducing a layer of points
ggplot(listings_df, aes(x=Review_scores_rating_vector, y=Price_Vector)) + geom_point()
#using geom_smooth(method=lm) because of its fit with linear data
ggplot(listings_df, aes(x=Review_scores_rating_vector, y=Price_Vector)) + geom_point() + geom_smooth(method=lm)
  