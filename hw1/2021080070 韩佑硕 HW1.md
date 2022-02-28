# Algorithm for graph sampling from large graph

## Type of 








# Correlation Analysis
### Import libraries
```R
library(DescTools)
library(dplyr)
library(ggplot2)
library(corrplot)
```

### import user_data_csv
I changed columns' name each to *"age"* and *"time"* for convenience.
```R
user_data<- read.csv("/Users/wooseokhan/Desktop/datascience/hw1/user_data.csv")
colnames(user_data) = c("age","time")

```
## 1. Data Cleaning
```R
#create dataframe
user_data_ex <- user_data
df_data <-data.frame(user_data_ex)
```
### 1-1 *"Age"* column data cleaning
I did data cleaning for both columns.
```R
# change class as numeric
age <- user_data_ex[,1]
df_data$age <- as.numeric(age)
```
First, check how many Na are there
```R
#check Na
table(is.na(df_data$age)) 
```
we get 4 NAs, because there are 1000 rows total, its better to eliminate Na rows 
```R
# Na removal
df_data <- df_data %>% filter(!is.na(age))
```
But we can also replace NULL with some representative data -- mode,mean...etc. here, I will use mode.
Here is how I get mode
```R
#get mode
age_num <- df_data$age
age_num_mode <-Mode(age_num)
age_num_mode[1]
```
and replce Na.
```R
# Replacement
df_data_2 <-data.frame(user_data_ex)
df_data_2$age <-as.numeric(age)
x <-df_data_2$age
df_data_2$age <- ifelse(is.na(x),age_num_mode[1],x)
```
Next, I will remove outliers, which affects correlation coefficient in a bad way, especially with Pearson's correlation coefficient, that I will adapt later.

Here, I adapted IQR Rule to detect and define outlier. I did it both for *df_data*(Na removed) and *df_data_2*(Na replaced by representative data)
```R
# Detecting outliers
age_outlier <-boxplot.stats(df_data$age)$out
age_outlier # <=2.0, >=47.0
df_data$age <- ifelse(df_data$age>46.0,NA,ifelse(df_data$age<3.0,NA,df_data$age))
df_data <- df_data %>% filter(!is.na(age))
****
df_data_2$age <- ifelse(df_data_2$age>46.0,NA,ifelse(df_data_2$age<3.0,NA,df_data_2$age))
df_data_2 <- df_data_2 %>% filter(!is.na(age))
```
### 1-2 *"Time"* column data cleaning
It's similar with age column data cleaning.
```R
# Change class of numeric
df_data$time <- as.numeric(df_data$time)
df_data_2$time <- as.numeric(df_data_2$time)
```
There was no Na in *"time"* column, thus I only did outlier removal.
```R
# time outlier processing // IQR Rule
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

```
## 3. Evaluation of data cleaning
I draw two boxplots, before and after data cleaning, to evaluate whether my data cleaning process is appropriate process. Since I applied data cleaning for two columns, there are 4 boxplots overall.
### 3-1 *age* boxplot
```R
age <- as.numeric(age)
boxplot(age,df_data$age,
        col = c("red","blue"),
        main = "Age of users",
        xlab = "Age",
        ylab = "number",
        ylim = c(0,100),
        names = c("before_age","after_age")
        )
```
### 3-2 *time* boxplot
```R
boxplot(time, df_data$time,
        col = c("red","blue"),
        main = "time of use per day",
        xlab = "time",
        ylab = "number",
        ylim = c(0,150),
        names = c("before_time","after_time")
)
```
Also, I compared the size of data after cleaning to see whether there was too much removal so that can affect the analyzing process, which shouldn't happen.
```R
before = nrow(data.frame(user_data_ex)) #1000
after = nrow(df_data) #965
percent of removed data = (1-(after/before))*100 #3.5%
```
There were only 25 datas removed, which is 3.5% of whole data, so it's trivial.

## 4. Correlation Analysis 
#### **I only covered *df_data*(Na removed)**

First, I did normality test to check whether the given data has been drawn from a normally distributed population. I used *Q-Q plot* for normality test, for it's convinent and very intuitive.
```R
# Normality test - QQ plot
qqnorm(df_data$age)
qqnorm(df_data$time)
```
From the graph,we can easily find that both data follow normal distribution. Now,we can use *Pearson correlation coefficient* to explain the relationship between user's *age* and *time of use per day*!!


We can also check the data follows normal distribution by drawing *probability density function*.
```R
# Density plot
ggplot(data=df_data)+
  geom_density(mapping =aes(x=age),colour = 'blue')+
  labs(subtitle = "Relationship between age and time",
       y="density",
       x="age(year)",
       title="Density line",
       caption = "Source : datascienceclass")
```
Next, I calculated *covariance* to roughly know the relationship between two variables, before specifically explore it.
```R
# Covariance
cov(df_data$time,df_data$age) #109.5592
```
Since covariance is 109.5592, we can think that two variables are positively related, and they move in the same direction in general.

Finally, let's calculate *pearson correlation coefficient*
```R
cor(df_data$age,df_data$time,use='complete.obs',method='pearson') #0.7456772
```
The result is .... 0.7456772!!! It says there is a **strong positive linear relationship** between these two variables( age and time).

Through drawing *soothing line* and *scatter plot*, we can double-check it.
```R
# Soothing line + scatter plot
ggplot(data=df_data,aes(x=age,y=time))+
  geom_smooth(method='auto',se=F)+
  geom_point(mapping = aes(x=age,y=time))+
  labs(subtitle = "Relationship between age and time",
       y="time(min)",
       x="age(year)",
       title="Scatterplot and Soothing line",
       caption = "Source : datascienceclass")
```
Before end, I also checked p-value to check whether this relationship is credential.
```R
# p-value
cor.test(df_data$age,df_data$time,use='complete.obs',method='pearson') # p-value < 2.2e-16
```
Thus, thie relationship is credential.

