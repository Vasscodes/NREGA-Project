#The following report provides an understanding of MNREGA data
#of different districts of differrent states. The report analyses how
#different variables are correlated and how variable like 












#loading data 
getwd()
setwd("C:/Users/Vasavi/Desktop/R project")

Data <- read.csv('NREGA.csv')

#glimpse of the data
install.packages('tidyverse')
library(tidyverse)
glimpse(Data)
summary(Data)

#summary of data along with info about missing values
install.packages('dplyr')
install.packages('skimr')
library(skimr)
skim(Data)

#cleaning the data 
installed.packages('dplyr')
library(dplyr)
Data <- distinct(Data)
Data <- na.omit(Data)
Data

###Exploratory data analysis###
head(Data)
summary(Data$Total.No..of.JobCards.issued)
summary()

#changing columns 3 to 30 to numeric values 
Data[3:30] <- lapply(Data[3:30], as.numeric)
Data
glimpse(Data)

#renaming from columns 3 to 30 as my data
my_data <- Data[3:30]

#rounding off the correlation matrix to 2 decimal points
corr_Data <- round(cor(my_data), 2)
head(corr_Data, 2)

#plot only the upper triangular heatmap
corr_Data[lower.tri(corr_Data)] <- NA 

#drop perfect correlations
corr_Data[corr_Data == 1] <- NA 

#plot only the lower triangular heatmap
corr_Data[upper.tri(corr_Data)] <- NA 

#installing packages: reshape is used to reshape the dataset in the 'wide' format or vice-versa
install.packages('reshape2')
library(reshape2)

#reduce the size of correlation matrix
melted_corr_Data <- melt(corr_Data)

#creating a correlation heatmap using ggplot2 
install.packages('ggplot2')
library(ggplot2)

#plotting the heatmap
ggplot(data = melted_corr_Data, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5))
 
###Exploratory Data Analysis###

#States included under MNREGA scheme 
unique(Data$state_name)

#districts included under MNREGA scheme 
unique(Data$district_name)

#sd and summary of SC and ST
#here the difference between min and max value is large, the std deviation here is high
#and indicates there are more data further away from the mean.
summary(Data$SC.workers.against.active.workers)
sd(Data$SC.workers.against.active.workers)

#for ST
#here the range is high and the standard deviation is also high i.e. more values 
#lie away from the mean.
summary(Data$ST.workers.against.active.workers)
sd(Data$ST.workers.against.active.workers)

#Data Visualisation and Data Analysis through Questions
##Question 1
##Which state has the highest no. of active ST and SC workers?

#plotting the state wise no of ST workers
ggplot(data = Data, aes(x=state_name, y=ST.workers.against.active.workers)) + geom_bar(stat = 'identity', fill = 'orange') + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) + coord_flip() + ggtitle('Total No. of ST workers in each state') + theme_dark() + scale_y_continuous(labels = scales::comma, limits = c(0,4000000))
###Observation: we can therefore observe that Madhya Pradesh has the highest no. of ST workers followed by
#Rajasthan and Odisha.
#we can also observe that states like Punjab and Haryana do not have ST workers and also 
#union territories like Puducherry, Lakshdweep and Dadra and Nagar Haveli have no ST workers

#plotting the state wise no. of SC workers
ggplot(data = Data, aes(x=state_name, y=SC.workers.against.active.workers)) + geom_bar(stat = 'identity', fill = 'orange') + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5)) + coord_flip() + ggtitle('Total No. of SC workers in each state') + theme_dark() + scale_y_continuous(labels = scales::comma, limits = c(0,4000000))
###Observation: we can therefore observe that West Bengal has the hgihest no of SC workers followed by 
#Uttar Pradesh. Rajasthan and Tamil Nadu. States like Goa, Arunachal Pradesh, Nagaland and Mizoram have no SC workers.
#Union Territories like Lakshdweep, Ladakh, Dadar and Nagar Haveli and Andaman and Nicobar Islands.
#Mizoram which has highest ST population has very less ST workers including Arunachal Pradesh



##Question 2
##What the difference between the no of job cards issued and the no of active job cards depict?
library(ggplot2)
#visualising the total no of job cards issued in each state
plot_1 <- ggplot(data = Data, aes(x=state_name, y=Total.No..of.JobCards.issued)) + geom_bar(stat = 'identity', fill = 'orange') + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), axis.text.y = element_text(angle = 270, hjust = 0, vjust = 0.5)) + coord_flip() + ggtitle('Total no. of job cards issued') + theme_dark() + scale_y_continuous(labels = scales::comma)

plot_1

#visualising the total no of active job cards
plot_2 <- ggplot(data = Data, aes(x=state_name, y=Total.No..of.Active.Job.Cards)) + geom_bar(stat = 'identity', fill = 'orange') + theme(axis.text.x = element_text(angle = 270, hjust = 0, vjust = 0.5), axis.text.y = element_text(angle = 270, hjust = 0, vjust = 0.5)) + coord_flip() + ggtitle('Total no. of active job cards') + theme_dark() + scale_y_continuous(labels = scales::comma, limits = c(0,15000000))

plot_2

#installing patchwork package to view both the graphs side by side.
install.packages('patchwork')
library(patchwork)

plot_1 + plot_2
###Observation: States like Tamilnadu, Bihar and Maharashtra have very less active job cards
#against the the no of job cards issued i.e. very less no of MNREGA workers with issued job cards
#are actively working.
#We can also observe that while Bihar has one of the highest no of job cards issued but then
#the active no of job cards is low. All states have less no of active job cards against the no of job cards issued

##Question 3
##How the expenditure of MNREGA has been spent in each state?

#grouping the actual data based on state and taking the mean of all the districts for each expenditure 
Data2 <- Data %>%
  group_by(state_name) %>%
  summarise(avg_NRM_expenditure = mean(X..of.NRM.Expenditure.Public...Individual.),
            avg_category_B_works = mean(X..of.Category.B.Works),
            avg_agriculture_expenditure = mean(X..of.Expenditure.on.Agriculture...Agriculture.Allied.Works),
            .groups = 'drop')
Data2

#visualising the data using plotly 
library(plotly)

fig <- plot_ly(Data2, x = ~state_name, y = ~avg_NRM_expenditure, type = 'bar', name = '% average expenditure on NRM') %>% 
  add_trace(y = ~avg_category_B_works, name = '% average expenditure on category B works') %>% 
  add_trace(y = ~avg_agriculture_expenditure, name = '% average expenditure on agriculture and allied works') %>% 
  layout(title = 'MNREGA Expenditure Analysis', yaxis = list(title = 'Count'), xaxis = list(tickangle=270),  barmode = 'group', bargap = 10)

fig
###Observasion:



##Question 4
##How much labour cost of women workers is the part of wage expenditure?

library(plotly)

Data3 <- Data %>%
  group_by(state_name) %>%
  summarise(women_total_earnings = sum(Women.Persondays*Average.Wage.rate.per.day.per.person.Rs../100000),
            total_wage = sum(Wages.Rs..In.Lakhs.),
            .groups = 'drop')
Data3

fig1 <- plot_ly(Data3, x = ~state_name, y = ~women_total_earnings, type = 'bar', name = 'Labour cost of women workers(Rs.)')%>% 
  add_trace(y = ~total_wage, name = 'Total Wage(Rs.)') %>% 
  layout(title = 'Labour vost of Women vs. Total Wage', yaxis = list(title = 'count'), xaxis = list(tickangle=270), barmode ='stack') 

fig1 
###Observation:



##Question 5
##What about the overall female labour force participation rate?

library(ggplot2)
library(plotly)


Data4 <- Data %>%
  group_by(state_name) %>%
  summarise(women_persondays = sum(Women.Persondays),
            .groups = 'drop')
Data4

fig3 <- plot_ly(data = Data4, x= ~state_name, y= ~women_persondays, type = 'scatter', marker = list(size = 15, color = 'orange')) %>%
  layout(title = 'Women Labour Force Participation')
fig3

###Obervation:







##Question 6 
##
