# import libraries
library(DescTools)
library(dplyr)
library(ggplot2)
library(corrplot)
# read file
user_data<- read.csv("/Users/wooseokhan/Desktop/datascience/hw1/user_data.csv")
colnames(user_data) = c("age","time")

# Data Cleaning
user_data_ex <- user_data
df_data <-data.frame(user_data_ex)
#1-1 age Null
age <- user_data_ex[,1]
#1-1-1 change class as numeric
df_data$age <- as.numeric(age)
#1-1-2 check how many NULL there are 
table(is.na(df_data$age)) # 4 NULL
#1-1-3.1 Eliminate NULL because there are only 4 null in this data
df_data <- df_data %>% filter(!is.na(age))
df_data
#1-1-3.2 Or replace NULL with representative data - Mode
#1-1-3.2 get mode 
age_num <- df_data$age
age_num_mode <-Mode(age_num)
age_num_mode[1]
#1-1-3.2 Replace NULL with mode
df_data_2 <-data.frame(user_data_ex)
df_data_2$age <-as.numeric(age)
x <-df_data_2$age
df_data_2$age <- ifelse(is.na(x),age_num_mode[1],x)

#1-2 age outlier processing //IQR Rule
age_outlier <-boxplot.stats(df_data$age)$out
age_outlier # <=2.0, >=47.0
df_data$age <- ifelse(df_data$age>46.0,NA,ifelse(df_data$age<3.0,NA,df_data$age))
df_data <- df_data %>% filter(!is.na(age))

df_data_2$age <- ifelse(df_data_2$age>46.0,NA,ifelse(df_data_2$age<3.0,NA,df_data_2$age))
df_data_2 <- df_data_2 %>% filter(!is.na(age))

#2-1 time
#2-1-1 change class of numeric
df_data$time <- as.numeric(df_data$time)
df_data_2$time <- as.numeric(df_data_2$time)
#2-1-2 time outlier processing // IQR Rule
time_outlier <- boxplot.stats(df_data$time)$out
time_outlier # <=-1641/ >=122
time <-df_data$time
df_data$time <-ifelse(df_data$time>121.0,NA,ifelse(df_data$time<10.0,NA,df_data$time))
df_data <-df_data %>% filter(!is.na(time))
table(is.na(df_data$time))


time_outlier_2 <- boxplot.stats(df_data_2$time)$out
time_outlier_2 # <=-1641/ >=122
time_2<-df_data_2$time
df_data_2$time <-ifelse(df_data_2$time>121.0,NA,ifelse(df_data_2$time<10.0,NA,df_data_2$time))
df_data_2 <-df_data_2 %>% filter(!is.na(time))
table(is.na(df_data_2$time))

#3 Evlaluation of data cleaning
#3-1 age
age <- as.numeric(age)
boxplot(age,df_data$age,
        col = c("red","blue"),
        main = "Age of users",
        xlab = "Age",
        ylab = "number",
        ylim = c(0,100),
        names = c("before_age","after_age")
        )
#3-2 time
boxplot(time, df_data$time,
        col = c("red","blue"),
        main = "time of use per day",
        xlab = "time",
        ylab = "number",
        ylim = c(0,150),
        names = c("before_time","after_time")
)
#3-3 size of data after cleaning
before = nrow(data.frame(user_data_ex)) #1000
after = nrow(df_data) #965
(1-(after/before))*100 #3.5%

# Correlation Analysis
#1.Normality test
qqnorm(df_data$age)
qqnorm(df_data$time)
#2.Density plot
ggplot(data=df_data)+
  geom_density(mapping =aes(x=age),colour = 'blue')+
  labs(subtitle = "Relationship between age and time",
       y="density",
       x="age(year)",
       title="Density line",
       caption = "Source : datascienceclass")
  
#3.Soothing line + scatter plot
ggplot(data=df_data,aes(x=age,y=time))+
  geom_smooth(method='auto',se=F)+
  geom_point(mapping = aes(x=age,y=time))+
  labs(subtitle = "Relationship between age and time",
       y="time(min)",
       x="age(year)",
       title="Scatterplot and Soothing line",
       caption = "Source : datascienceclass")
#4.Covariance
cov(df_data$time,df_data$age) #109.5592
#5. correlation coefficient
cor(df_data$age,df_data$time,use='complete.obs',method='pearson') #0.7456772
cor.test(df_data$age,df_data$time,use='complete.obs',method='pearson')

cor.test(df_data_2$age,df_data_2$time,use='complete.obs',method='pearson')
