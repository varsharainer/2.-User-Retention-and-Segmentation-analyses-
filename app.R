setwd("D:/FrontRow/")
getwd()
list.files()

frRawDF <- read.csv("frontRow_DAA.csv",header = T)
dim(frRawDF)
head(frRawDF)

frTranDF <- as.data.frame(t(frRawDF))

dauWithRep <- apply(frRawDF, 2, function(x) length(which(!is.na(x))))
class(dauWithRep)
dauWithRep

dauWithoutRep <- apply(frRawDF, 2, function(x) length(which(!is.na(unique(x)))))

dauWithoutRep - dauWithRep ####???

dates = seq(from = as.Date("2020-01-01"), to = as.Date("2020-03-31"), by = 'day')
dates
View(dates)

frTranDF$Dates <- dates
plot(dauWithoutRep~dates, type="l", ylab="Daily Active Users", xlab="Day", col="red")

head(frTranDF)
names(frTranDF)

View(frTranDF)


frRawDFMessy <- read.csv("frontRow_DAA.csv",header = T) #### 
#View(frRawDFMessy)
names(frRawDFMessy)

require(tidyr)
frRawDFTidy <- frRawDFMessy %>% 
  gather(Dates, UID, X01.01.2020:X31.03.2020, na.rm = T)

str(frRawDFTidy)

length(unique(frRawDFTidy$UID)) ## 4594
n_occur <- data.frame(table(frRawDFTidy$UID))
dim(n_occur)
colnames(n_occur) <- c("UID","Life_Time_Activity")
head(n_occur)
View(n_occur)
oneTimeVisitors <- n_occur[n_occur$Life_Time_Activity== 1,]
View(oneTimeVisitors)

require(dplyr)
frRawDFTidy <- frRawDFTidy %>% mutate(Dates = as.character(gsub("X", "", Dates)))
head(frRawDFTidy)
str(frRawDFTidy)

frRawDFTidy$Dates <- as.Date(frRawDFTidy$Dates, format="%d.%m.%Y")
head(frRawDFTidy)
str(frRawDFTidy)


#Daily Retension Rate Day(i) - Day(j)

require(lubridate)
dailyRetension <- expand.grid(date1 = unique(frRawDFTidy$Dates),                             ## create all combinations between dates
                              date2 = unique(frRawDFTidy$Dates)) %>%
  filter(date1 < date2) %>%                                        ## keep only cases where 2nd date is after 1st date
  group_by(date1, date2) %>%                                       ## for each combination of dates
  do({UIDs_1 = setdiff(unique(frRawDFTidy[frRawDFTidy$Dates == ymd(.$date1),]$UID), ## get new ids in date1 (they showed up first time at this date)
                       unique(frRawDFTidy[frRawDFTidy$Dates < ymd(.$date1),]$UID))           
  N_UIDs_1 = length(UIDs_1)                                          ## count how many ids you have
  UIDs_2 = unique(frRawDFTidy[frRawDFTidy$Dates == ymd(.$date2),]$UID)              ## get ids from date2
  N_UIDs_2 = length(intersect(UIDs_2, UIDs_1))                        ## count how many ids exist in previous dataset
  data.frame(Prc = N_UIDs_2/N_UIDs_1)}) %>%                          ## calculate the percentage          
  ungroup()

View(dailyRetension)

write.csv(dailyRetension,"dailyRetension.csv")

frRawDFTidy <- frRawDFTidy %>%
  group_by(UID)%>%
  mutate(date_of_first_engagement=min(Dates))

head(frRawDFTidy)

frRawDFTidy <- frRawDFTidy%>%
  mutate(Users_Status = case_when(Dates>date_of_first_engagement ~ "Returning",
                                  Dates == date_of_first_engagement ~ "New"))

head(frRawDFTidy)
View(frRawDFTidy)


New_and_Returning_Users <-  frRawDFTidy%>%
  group_by(floor_date(Dates,unit = 'day'))%>%
  summarise(New_Users = n_distinct(UID[Users_Status=="New"]),
            Returning_Users= n_distinct(UID[Users_Status=="Returning"]))

colnames(New_and_Returning_Users)[1] <- "Dates"

New_and_Returning_Users
View(New_and_Returning_Users)
New_and_Returning_Users$Total_Users <- New_and_Returning_Users$New_Users + New_and_Returning_Users$Returning_Users

require(ggplot2)
ggplot(New_and_Returning_Users, aes(dates)) + 
  geom_line(aes(y = New_Users, colour = "New_Users")) + 
  geom_line(aes(y = Returning_Users, colour = "Old_Users"))+
  ylab('Users')+xlab('Dates')

str(New_and_Returning_Users)

New_and_Returning_Users$newUsersProp <- New_and_Returning_Users$New_Users/ New_and_Returning_Users$Total_Users
New_and_Returning_Users$oldUsersProp <- New_and_Returning_Users$Returning_Users/ New_and_Returning_Users$Total_Users 


head(New_and_Returning_Users)

#  Get months
New_and_Returning_Users$Month <- month(New_and_Returning_Users$Dates)
New_and_Returning_Users$Week <- week(New_and_Returning_Users$Dates)
#week(New_and_Returning_Users$Dates)
View(New_and_Returning_Users)

monthlyOldUsersProp <- aggregate( oldUsersProp ~ Month,  New_and_Returning_Users, mean )
weeklyOldUsersProp <- aggregate( oldUsersProp ~ Month+Week,  New_and_Returning_Users, mean )

monthlyNewUsersProp <- aggregate( newUsersProp ~ Month,  New_and_Returning_Users, mean )
weeklyNewUsersProp <- aggregate( newUsersProp ~ Month+Week,  New_and_Returning_Users, mean )


combinedMonthlyProp <- merge(monthlyOldUsersProp,monthlyNewUsersProp)
ggplot(combinedMonthlyProp,aes(Month, group=1)) + 
  geom_line(aes(y = oldUsersProp, colour = "oldUsersProp")) + 
  geom_line(aes(y = newUsersProp, colour = "newUsersProp"))+
  ylab('Old Vs New Users')+xlab('Month')

combinedWeeklyProp <- merge(weeklyOldUsersProp,weeklyNewUsersProp)
ggplot(combinedWeeklyProp,aes(Week, group=1)) + 
  geom_line(aes(y = oldUsersProp, colour = "oldUsersProp")) + 
  geom_line(aes(y = newUsersProp, colour = "newUsersProp"))+
  ylab('Old Vs New Users')+xlab('Weeks')

ggplot(New_and_Returning_Users,aes(Dates, group=1)) + 
  geom_line(aes(y = oldUsersProp, colour = "oldUsersProp")) + 
  geom_line(aes(y = newUsersProp, colour = "newUsersProp"))+
  ylab('Old Vs New Users')+xlab('Dates')

tidy_clean <-   frRawDFTidy  ### Save a copy of tidyUserData in tidy_clean 
tidy_clean$UID<-as.character(tidy_clean$UID)
max_date<- max(tidy_clean$Dates)
class(max_date)
class(tidy_clean$Dates)


#Recency

recency<- tidy_clean %>% group_by(UID) %>% summarise(last_date= max(Dates)) %>%
  mutate(recency= as.numeric(max_date -last_date))

View(recency)
Users_Recency <- recency[,c(1,3)]
User_Frequency <- n_occur
Users_RFTest<- merge(Users_Recency, User_Frequency)

ggplot(Users_RFTest,aes(as.numeric(UID), group=1)) + 
  geom_line(aes(y = recency, colour = "Recency")) + 
  geom_line(aes(y = Life_Time_Activity, colour = "Life_Time_Activity"))+
  ylab('User Retention')+xlab('UID')

DT::datatable((Users_RFTest),
              rownames = FALSE,
              options = list(
                pageLength = 10))

# Creating Clusters based on the RF Table using Unsupervised Statistical
set.seed(415)
clusters <- kmeans(scale(Users_RFTest[,2:3]), 3, nstart = 1) # Performing kmeans with RF variables and creating 3 clusters. 

Users_RFTest$Cluster <- as.factor(clusters$cluster) # Attaching the results to CustomersID to identify each customer's cluster

View(Users_RFTest)
str(Users_RFTest)
class(Users_Recency)
KMeans_Results <- Users_RFTest %>%
  group_by(Cluster) %>%
  summarise('Number of Users' = n(),
            'Recency Mean' = round(mean(recency)),
            'Frequency Mean' = round(mean(Life_Time_Activity)))

DT::datatable((KMeans_Results),
              rownames = FALSE) # Display cluster means to identify their value to the business

 
##### The End :: Thank You