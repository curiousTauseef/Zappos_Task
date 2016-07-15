# Name: Shrutik
# Zappos Internship Task
# 

install.packages("ggplot2")
library(ggplot2)

install.packages("randomForest")


library(devtools)
library(reshape)
library(plyr)
library(randomForest)

data1 = read.csv(file = "C:/Users/Shrutik/Desktop/Zappos/Analytics Challenge Data 2.csv", sep = ",")

str(data1)
#fix(data1)
mean(data1$gross_sales, na.rm = T) # Mean of Gross Sales

ggplot(data1, aes(x = gross_sales)) + geom_histogram(col = "darkblue" , fill = "cornflowerblue" , alpha = 0.7, bins = 7)+ ggtitle("Histogram of Gross Sales")

data1 <- data1[,-13:-15]

str(data1$day)

data1$day <- as.Date(data1$day, format = "%m/%d/%Y")

str(data1)

table(data1$site)
table(data1$platform)

head(data1)

table(data1$site, data1$new_customer) # Percentage of new customers on each site

table(data1$platform, data1$new_customer) # Percentage of new customers on each platform

data2 = data1
data2$product_search_ratio = (data1$product_page_views/data1$search_page_views) # In a perfect world, ratio would be 1
str(data2$product_search_ratio)

data2$product_search_ratio[is.infinite(data2$product_search_ratio)] <- NA # Converting infinity to NA
hist(data2$product_search_ratio)
# Removing all the NAs
for(i in 1:ncol(data2)){
  data2[is.na(data2[,i]), i] <- mean(data2[,i], na.rm = TRUE)
}

str(data2)

data2$rounded_product_search_ratio <- round(data2$product_search_ratio, digits = 1) # Rounding the ratio to the nearest 10th
str(data2)

table(data2$platform,data2$rounded_product_search_ratio)

# Conversion Ratio:
# 
ratio_df <- data.frame(Platform =  c('Android','BlackBerry','ChromeOS','iOS','iPad','iPhone','Linux','Macintosh','MacOSX','Windows','WindowsPhone'),

Mean = c(mean(data2$product_search_ratio[data2$platform=='Android'])
, mean(data2$product_search_ratio[data2$platform=='BlackBerry'])
, mean(data2$product_search_ratio[data2$platform=='ChromeOS'])
, mean(data2$product_search_ratio[data2$platform=='iOS'])
, mean(data2$product_search_ratio[data2$platform=='iPad'])
, mean(data2$product_search_ratio[data2$platform=='iPhone'])
, mean(data2$product_search_ratio[data2$platform=='Linux'])
, mean(data2$product_search_ratio[data2$platform=='Macintosh'])
, mean(data2$product_search_ratio[data2$platform=='MacOSX'])
, mean(data2$product_search_ratio[data2$platform=='Windows'])
, mean(data2$product_search_ratio[data2$platform=='WindowsPhone'])
)
)
str(ratio_df)
ratio_df

g = ggplot(ratio_df,aes(x = Platform, y = Mean)) + geom_bar(stat="identity", col = "darkblue" , fill = "cornflowerblue" , alpha = 0.8) 
g1 = g  + theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))

table(data1$platform)

ggplot(data1, aes(x=gross_sales,y=platform)) + geom_line() # Convert to bar
mean(data1$gross_sales[data1$platform=='Android'], na.rm = T)
mean(data1$gross_sales[data1$platform=='Windows'], na.rm = T)

# New Customers:

mean(data1$gross_sales[data1$new_customer==1],na.rm = T) # Mean Gross Sales for new Customers
mean(data1$gross_sales[data1$new_customer==0],na.rm = T) # Old Customers

new_cust_platform <- as.data.frame.matrix(table(data1$platform,data1$new_customer)) 

new_cust_platform <- cbind(rownames(new_cust_platform), new_cust_platform)
rownames(new_cust_platform) <- NULL
colnames(new_cust_platform) <- c("COL1","Old Customer","New Customer")
new_cust_platform

melted_new_cust_platform = melt(new_cust_platform, id = "COL1")

ggplot(melted_new_cust_platform, aes(COL1, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1))

#########################################
#########################################
# Visists Map:
# 
#
str(data1)
day_site_visits <- data.frame(data1$day,data1$site,data1$visits)
colnames(day_site_visits) <- c('day','site','visits')
str(day_site_visits)
# Counting visits per day
lgrep <- function(day_site_visits,pat)
{ 
  d = sum(day_site_visits$visits[day_site_visits$site==pat])
  return((d))
}


counts.Acme <- ddply(day_site_visits, .(day),  "lgrep", pat="Acme", .progress = "text")
counts.Botly <- ddply(day_site_visits, .(day), "lgrep", pat="Botly", .progress="text")
counts.Pinnacle <- ddply(day_site_visits, .(day), "lgrep", pat="Pinnacle", .progress="text")
counts.Sortly <- ddply(day_site_visits, .(day), "lgrep", pat="Sortly", .progress="text")
counts.Tabular <- ddply(day_site_visits, .(day), "lgrep", pat="Tabular", .progress="text")
counts.Widgetry <- ddply(day_site_visits, .(day), "lgrep", pat="Widgetry", .progress="text")

df <- data.frame(
  day=counts.Acme$day
  , Acme=counts.Acme$lgrep
  , Botly = counts.Botly$lgrep
  , Pinnacle=counts.Pinnacle$lgrep
  , Sortly=counts.Sortly$lgrep
  , Tabular=counts.Tabular$lgrep
  , Widgetry=counts.Widgetry$lgrep) 

str(df)
head(df)
df$day <- as.Date(df$day, format = "%m/%d/%Y")
# fix(df)
em <- melt(df, id = "day")
str(em)

as.Date(em$day)
str(data1)

ggplot(aes(as.Date(day), value, color = variable), colour=clarity , data=em) + 
  scale_x_date('') + 
  stat_smooth() + 
  scale_y_continuous('visits') + 
  geom_line(alpha=0.25) +  
  geom_point(alpha=0.20) + 
  scale_colour_brewer(palette="Set1")






# Let's Zoom into Fall and Consider only  Botly, Pinnacle, Sortly, Tabular and Widgetry

length(data1$day[data1$day>'2013-08-01'])

head(df)
str(df)


fall_df <- data.frame(df[df$day>'2013-08-01',])

str(fall_df)
head(fall_df)

fall_df <- subset(fall_df, select = c('day','Botly','Pinnacle','Sortly','Tabular','Widgetry'))

str(fall_df)
head(fall_df)

melted_fall_df <- melt(fall_df, id = "day")
head(melted_fall_df)

ggplot(aes(as.Date(day), value, color = variable), colour=clarity , data=melted_fall_df) + 
  scale_x_date('') + 
  stat_smooth() + 
  scale_y_continuous('visits') + 
  geom_line(alpha=0.25) +  
  geom_point(alpha=0.2) + 
  scale_colour_brewer(palette = "Set1")




##########################################################
#########################################
##########################################################
##########################################################

str(data1)

ggplot(data1, aes(x=site, y=gross_sales)) + geom_point(aes(color = platform))


## Android Sales:

table(data1$platform)

sum(data1$gross_sales[data1$platform=='Android'],na.rm = T)





platform_sales <- data.frame(Platform =  c('Android','BlackBerry','ChromeOS','iOS','iPad','iPhone','Linux','Macintosh','MacOSX','Windows','WindowsPhone'),
                       
                       sum = c(sum(data1$gross_sales[data1$platform=='Android'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='BlackBerry'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='ChromeOS'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='iOS'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='iPad'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='iPhone'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='Linux'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='Macintosh'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='MacOSX'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='Windows'],na.rm=T)
                                , sum(data1$gross_sales[data1$platform=='WindowsPhone'],na.rm=T)
                       )
)
str(platform_sales)

g3 = ggplot(platform_sales,aes(x = Platform, y = sum)) + geom_bar(stat="identity", col = "darkblue" , fill = "cornflowerblue" , alpha = 0.8) 
g3  + theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1)) #2




##############################################################
##############################################################
##############################################################
##############################################################

str(data1)

ggplot(data1, aes(x=site, y=gross_sales)) + geom_point(aes(color = platform)) #1

g3 = ggplot(platform_sales,aes(x = Platform, y = sum)) + geom_bar(stat="identity", col = "darkblue" , fill = "cornflowerblue" , alpha = 0.8) 
g3  + theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust = 1)) #2

ggplot(data1,aes(factor(site))) + geom_bar() #3

ggplot(data1,aes(orders,gross_sales)) + geom_point(aes(color = site)) #5

ggplot(data1,aes(orders,gross_sales)) + geom_point(aes(color = platform)) #6


ggplot(data1,aes(product_page_views,gross_sales)) + geom_point() + geom_smooth(method = "lm") #7

ggplot(data1,aes(search_page_views,gross_sales)) + geom_point(aes(color = platform)) #8

ggplot(data1,aes(search_page_views,gross_sales)) + geom_point(aes(color = site)) #9

ggplot(data1,aes(site,add_to_cart)) + geom_point() #10

ggplot(data1,aes(add_to_cart,platform)) + geom_point(aes(color=platform)) #11

ggplot(data1,aes(add_to_cart,gross_sales)) + geom_point() + geom_smooth(method = "lm") #12





#############################
# Histogram of Gross Sales
# The data here is left skewed, so we need to transform it
#  That is the reason why i took a log
temp = data1$gross_sales
hist(temp)
head(temp)
temp = data.frame(temp)
temp = log(temp)
ggplot(temp, aes(x = temp)) + geom_histogram(col = "darkblue" , fill = "cornflowerblue" , alpha = 0.7)+ ggtitle("Histogram of Gross Sales")


# Linear Regression

model1 = lm(formula = (gross_sales) ~ visits, data = data1)
summary(model1)

model2 = lm(formula = (gross_sales) ~ platform, data = data1)
summary(model2)

model3 = lm(formula = (gross_sales) ~ new_customer, data = data1)
summary(model3)


model4 = lm(formula = (gross_sales) ~ site + new_customer + platform + bounces + product_page_views, data = data1)
summary(model4)

predict(model4, newdata = data.frame(site='Acme',new_customer=1,platform='Windows',bounces=2,product_page_views=10))



#abline(lm(gross_sales ~ visits, data = data1))
# cor, pairs, summary

########################################################################################
########################################################################################
########################################################################################

# Random Forest
# 

length(data1$day)*2/3
train <- data1[1:14040,]
test <-  data1[14041:length(data1$day),]

train = na.omit(train)
test = na.omit(test)
str(train)
str(test)

rfm <- randomForest(gross_sales ~., train)
rfm
p <- predict(rfm,test)
mean(test[,5]==p)
(importance(rfm))
getTree(rfm,500,labelVar=T)



########################################################
########################################################
########################################################
########################################################
# RFM:
str(data1)

data3 = data.frame(data1$day,data1$gross_sales)
str(data3)

# Now we do not have UserID, so I will randomly assign User IDs

data3$UserID = sample(1000:9999,replace=T,size=length(data3$data1.day))
str(data3)

names(data3) = c('Date','Sales Value','UserID')

data3$recency=round(as.numeric(difftime(Sys.Date(),data3[,1],units="days")) )
str(data3)

## Creating Total Sales(Monetization),Frequency, Last Purchase date for each customer

salesM=aggregate(data3[,2],list(data3$UserID),sum)
str(salesM) # Sales Monetization
names(salesM)=c("UserID","Monetization")

salesF=aggregate(data3[,2],list(data3$UserID),length)
str(salesF) # Sales Frequency
names(salesF)=c("UserID","Frequency")

salesR=aggregate(data3[,4],list(data3$UserID),min)
str(salesR) # Sales Recency
names(salesR)=c("UserID","Recency")

##Merging R,F,M

test1=merge(salesF,salesR,"UserID")

salesRFM=merge(salesM,test1,"UserID")
str(salesRFM)

##Creating R,F,M levels 

salesRFM$rankR=cut(salesRFM$Recency, 5,labels=F) #rankR 1 is very recent while rankR 5 is least recent

salesRFM$rankF=cut(salesRFM$Frequency, 5,labels=F)#rankF 1 is least frequent while rankF 5 is most frequent

salesRFM$rankM=cut(salesRFM$Monetization, 5,labels=F)#rankM 1 is lowest sales while rankM 5 is highest sales

##Looking at RFM tables

table(salesRFM[,5:6])
table(salesRFM[,6:7])
table(salesRFM[,5:7])

