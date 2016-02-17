#' Reading the given dataset into R and naming it CTR_data
CTR_data <- read.csv("C:/R/debdutta/Dataset.csv", header=T, stringsAsFactors=F, strip.white=T)

#' Data preprocessing
#' Cleaning the variable names
names(CTR_data) <- tolower(gsub("\\.", "", names(CTR_data)))

#' Creating new variables and adding it to the dataset
#' Splitting the "postabout" variable to create a new variable "brand"
spl <- strsplit(CTR_data$postabout, " ")
first <- function(x){x[1]}
brand <- sapply(spl, first)

library(lubridate)

#' Creating a new variable "weekday" which is the corresponding day of the week for "startdate" variable
startdate <- mdy(CTR_data$startdate)     
weekday <- wday(startdate)

#' Adding the newly created variables to the dataset
CTR_data <- cbind(CTR_data, brand, weekday)

library(dplyr)

#' Renamed two lengthy-named variables "clickthroughratectr" & "frequencyperuser"
CTR_data <- rename(CTR_data, ctr = clickthroughratectr, freq = frequencyperuser)

#' Creating the final dataset by removing the unimportant variables
drop <- c("startdate", "postabout", "impressions", "clicks")
CTR_data <- CTR_data[,!names(CTR_data) %in% drop]

#' Fitting a linear regression model with ctr as the response variable and the rest of the variables as predictors (except website)
lm.fit <- lm(ctr~. -website, data=CTR_data)
summary(lm.fit)

#' Analysis of the linear regression results
#' ctr is chosen as the response variable rather than postlikes, postcomments or postshares individually as a higher ctr would lead to a consolidated increase of likes, comments and shares and hence, improve social presence.
#' website variable is excluded from the model as it is not generally correlated to ctr.
#' reach and postlikes are highly significant in the model. It can be attributed to the fact that posts with more likes attract more ctr & hence, further likes, comments and shares. 
#' reach variable, though highly significant, has a negative impact on ctr, which is a bit unlikely.
#' As we know, ctr = clicks/impressions and impressions = reach * freq, therefore, ctr = clicks/(reach * freq).
#' So, it might be the case that with the increase in reach, the denominator increases tremendously, but the numerator doesn’t increase proportionately, reducing ctr.
#' brandblackberry variable is significant at 10% level indicating that it is the most popular among the given brands and hence, attracts a higher ctr.
#' Intercept is also significant in the model indicating that there might be other important predictors, highly correlated with ctr, missing from the model. One of them could be the price of the products.

#' Partitioning the data
library(caret)

#' Partitioning the data
#' Creating training and test datasets with 70% in training set and 30% in test set
inTrain <- createDataPartition(y= CTR_data$ctr, p= 0.7, list=F)   
training <- CTR_data[inTrain,]              
testing <- CTR_data[-inTrain,]

#' Fitting a regression tree with ctr as the response variable and the rest of the variables as predictors (except website)
set.seed(101)                                              
modFit <- train(ctr~. -website, method="rpart", data=training)
print(modFit$finalModel)

#' Measuring model performance
#' Predicted values of ctr on the test dataset
yhat.ctr <- predict(modFit, newdata = testing)         
ctr.test <- testing[,1]

#' Calculating mean-squared error(MSE)
mean((yhat.ctr - ctr.test)^2)

#' The test MSE associated with the regression tree is 4.68. The square root of the MSE is therefore around 2.16, indicating that this model leads to test predictions that are within around 2.16 of the true mean ctr.
  
#' Plotting the regression tree
library(rattle)                 
fancyRpartPlot(modFit$finalModel)
  
#' Analysis of the regression tree results
#' reach is the most important variable followed by postlikes.
#' Posts shown to very less (< 574) yet a targeted group of people attract very high ctr (12).
#' Posts shown to (574 < number of people < 163744) and postlikes > 1064 also attracts high ctr (6).
  
#' Data visualization for business insights
library(ggplot2)                                                  
plot1 <- qplot(reach,ctr, colour= brand, data=training, main = "reach vs. ctr")
plot2 <- qplot(ctr,postlikes, colour= brand, data=training, main = "ctr vs. postlikes")
plot3 <- qplot(ctr,postcomments, colour= brand, data=training, main = "ctr vs. postcomments")
plot4 <- qplot(ctr,postshares, colour= brand, data=training, main = "ctr vs. postshares")

#' All four plots together
library(gridExtra)                                                 
grid.arrange(plot1, plot2, plot3, plot4, ncol=2)
  
#' The reach vs. ctr plot indicates that there is more concentration of ctr when reach is less.
#' From all the four plots above, it is evident that blackberry has the highest concentration of likes, comments and shares in the clusters, indicating that it is most popular among the given brands, followed by htc.
  
#' Clustering reach variable to identify the target group
#' Hierarchical clustering algorithm to identify the number of clusters of variable reach

attach(CTR_data)                                      
hc.complete <- hclust(dist(reach), method = "complete")    
plot(hc.complete)

#' Hierarchical clustering plot indicates that there are three main clusters

library(cluster)                      
library(fpc)                           
df <- data.frame(reach, ctr)  

#' k-means clustering algorithm with three clusters
clust <- kmeans(df, centers = 3)
plotcluster(df, clust$cluster, main = "reach vs. ctr")
  
#' Identified three distinct clusters of ctr by reach variable.
#' The highest concentration of ctr is at cluster 3, followed by clusters 1 & 2.
#' Cluster 3 is the target group of people for showing the posts to generate the highest ctr, followed by clusters 1 & 2.
  
#' Comparison of performance of websites A, B & C
#' Creating a new variable postactions (sum of postlikes, postcomments, postshares for each post) and adding it to the dataset
library(dplyr)                                             
combine <- select(CTR_data, postlikes:postshares) 
postactions <- apply(combine, 1, sum)
CTR_data <- cbind(CTR_data, postactions)
  
#' Variation of postactions of websites A, B & C over time
library(ggplot2)                                             
library(wesanderson)                                   
library(scales)

#' Converting startdate variable to date format from character format
startdate <- as.Date(startdate)
actions <- ggplot(CTR_data, aes(x = startdate, y = postactions, group = website)) +
scale_x_date(breaks = 'days', labels = date_format('%d')) +
geom_line(aes(colour = website), size=2) + scale_color_manual(values = wes.palette(3, "Zissou")) +
labs(y = "total actions per post", title = "Variation of postactions for A, B & C \n over time")
print(actions)
  
#' Average ctr and postactions for each website
library(igraph)                               
avctr <- ggplot(CTR_data, aes(website, ctr, colour = website, group = website, fill = website)) +
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average ctr for each website \n mean and 95% confidence interval", x = "", y = "Average ctr")
print(avctr)
  
avactions <- ggplot(CTR_data, aes(website, postactions, colour = website, group = website, fill = website))+
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average postactions for each website \n mean and 95% confidence interval", x = "", y = "Average # of actions")
print(avactions)
  
#' Contigency table of website vs. brand
table(CTR_data$website, CTR_data$brand)
  
#' Analysis of the performance comparison of websites
#' Over the time period of the case study, website C received more likes, comments and shares consolidated and hence, was more popular than A & B on mobileknowledge.com
#' Although Website B has the highest average ctr, Website C has the highest average postactions and hence, the most popular website.
#' A contingency table of website vs. brand reveals that Website C has the highest number of posts of brand “blackberry”, which is the most popular brand on mobileknowledge.com.
  
  
  
  