
library(dplyr) 
library(readxl)
library(tibble)
library(ggplot2)


#############################################################
####  Task 1 Titanic dataset & Understanding#################
#############################################################

#############################################################
####  Plotting survival rate of the titanic #################
#############################################################

titanic <- read_excel("E:/EBAC/FBA/Class 7 Data Exploration Basics/data/titanic/titanic3.xlsx")

str(titanic)

#changing the column name sex to gender 
names(titanic)[names(titanic) == "sex"] <- "gender"

#conversion of variables into factors
titanic$pclass= as.factor(titanic$pclass)
titanic$survived= as.factor(titanic$survived)
titanic$gender= as.factor(titanic$gender)
titanic$embarked= as.factor(titanic$embarked)

#Conversion of levels in survived from 0,1 to Died, Survied for better visibility
levels(titanic$survived)= c('Died','Survived')

#############################################################
####  Plotting survival rate of the titanic #################
#############################################################

titanic %>%
  group_by(survived) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(t))%>%
  ggplot(aes(x = as.factor(survived), y = count_level,fill=as.factor(survived) )) +
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,2)),vjust = 2, color="white")+
  labs(title = "Survival of Passengers in Titanic",
       subtitle = "Percentage of Passengers Died/Survived",
       x = "Died/Survived", 
       y = "Number of Passengers") +
  scale_fill_manual(values=c("#666666", "#003399"))

#############################################################
####  Plotting Gender Representation in Pessangers###########
#############################################################


titanic %>%
  group_by(gender) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(t))%>% 
  ggplot(aes(x = as.factor(gender), y = count_level,fill=as.factor(gender))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,2)),vjust = 2, color="white")+
  labs(title = "Gender Distribution",
       subtitle = "Percentage of Passengers Female/Male",
       x = "Female/Male", 
       y = "Number of Passengers") +
  scale_fill_manual(values=c("#666666", "#003399"))


#############################################################
####  Understanding gender impact ###########################
#############################################################

########## Hypothesis testing ###############################
tab1 = table(data$gender, data$survived)
addmargins(tab1)
chisq.test(tab1)

## X squared critical value for df = 3 is 7.815##############
#hence survival rate is depedent on embarkation Point #######


########## Visual Exploration ###############################

ggplot(data=t, aes(x=gender, fill=survived)) + 
  geom_bar(position = 'dodge')+
  labs(title = "Survival for Female and Male",
       subtitle = "Count of Female and Male Dead Survived",
       x = "Female/Male", 
       y = "Number of Passengers") +
  scale_fill_manual(values=c("#666666", "#003399"))

tab= table(titanic$gender, titanic$survived)
data1= as.data.frame.matrix(tab)
data1$survival_rate = data1$Survived/(data1$Died+data1$Survived)
data1 = rownames_to_column(data1, var="gender")
data1$gender=as.factor(data1$gender)
ggplot(data=data1, aes(x=gender, y=survival_rate))+
  geom_bar(fill = "#003399", stat = "identity")+
geom_text(aes(label=round(survival_rate,2)), 
          vjust=1.6, color="white", size=3.5)+
labs(title = "Survival Rates of Genders",
     subtitle = "Percentage of Died for each gender",
     x = "Female/Male", 
     y = "Survival Rate") +
  geom_hline(yintercept=0.38, linetype="dashed", 
             color = "#666666", size=2, 
             show.legend = "0.38")+
  geom_text(aes(2.1,0.40,label = "Overall Survival Rate is 0.38", vjust =0))



#############################################################
####  Understanding embarakation point impact################
#############################################################

#############################################################
#  Plotting embarakation point Representation in Pessangers##
#############################################################

#Removing 'NA' in embarked
t= titanic[!is.na(titanic$embarked),]

t %>%
  group_by(embarked) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(t))%>% 
  ggplot(aes(x = reorder(as.factor(embarked), -count_level), y = count_level,fill=as.factor(embarked))) +
  geom_bar(stat='identity') +
  geom_text(aes(label=round(percentage,2)),vjust = 2, color="white")+
  labs(title = "Embarkation Point Distribution",
       subtitle = "Percentage of Passengers from C/Q/S",
       x = "C Cherbourg/Q Queenstown/S Southampton", 
       y = "Number of Passengers") +
  scale_fill_manual(values=c("#666666", "#0066CC", "#003399"))


########## Hypothesis testing ###############################
tab = table(t$embarked, t$survived)
addmargins(tab)
chisq.test(tab)

## X squared critical value for df = 1 is 3.841##############
#hence survival rate is depedent on embarked#################


########## Visual Exploration ###############################

ggplot(data=t, aes(x=embarked, fill=survived)) + 
  geom_bar(position = 'dodge')+
  labs(title = "Survival of Pessangers",
       subtitle = "Count of Died/Survived based on Embarkation Point",
       x = "C Cherbourg/Q Queenstown/S Southampton", 
       y = "Number of Passengers")+
  scale_fill_manual(values=c("#666666", "#003399"))

tab2= table(titanic$embarked, titanic$survived)
data2= as.data.frame.matrix(tab2)
data2$survival_rate = data2$Survived/(data2$Died+data2$Survived)
data2 = rownames_to_column(data2, var="embarked")
data2$embarked=as.factor(data2$embarked)
ggplot(data=data2, aes(x=embarked, y=survival_rate))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(survival_rate,2)), 
            vjust=1.6, color="white", size=3.5)+
  labs(title = "Survival Rates",
       subtitle = "Percentage for each Embarked Point",
       x = "C Cherbourg/Q Queenstown/S Southampton", 
       y = "Survival Rate") +
  geom_hline(yintercept=0.38, linetype="dashed", 
             color = "#666666", size=2, 
             show.legend = "0.38")+
  geom_text(aes(2.3,0.40,label = "Overall Survival Rate is 0.38", vjust =0))

#############################################################
####  Understanding the impact of cabin #####################
#############################################################

#############################################################
#  Plotting embarakation point Representation in Pessangers##
#############################################################

#Removing 'NA' in cabin
t4= titanic
#we can't simple delete the NA's as it will discredit 77%of data
t4$cabinclass = substr(t4$cabin, 0, 1)
#let's replace all the Unknown with catagory 'U'
t4$cabinclass[is.na(t4$cabinclass)]="U"

########## Hypothesis testing ###############################
tab = table(t4$cabinclass, t4$survived)
addmargins(tab)
chisq.test(tab)

## X squared critical value for df = 8 is 21.955#############
#hence survival rate is depedent on embarked#################


########## Visual Exploration ###############################

ggplot(data=t4, aes(x=cabinclass, fill=survived)) + 
  geom_bar(position = 'dodge')+
  labs(title = "Survival of Pessangers",
       subtitle = "Count of Died/Survived based on Cabin Class",
       x = "Cabin Class", 
       y = "Number of Passengers")+
  scale_fill_manual(values=c("#666666", "#003399"))

tab4= table(t4$cabinclass, t4$survived)
data4= as.data.frame.matrix(tab4)
data4$survival_rate = data4$Survived/(data4$Died+data4$Survived)
data4 = rownames_to_column(data4, var="cabinclass")
data4$cabinclass=as.factor(data4$cabinclass)
ggplot(data=data4, aes(x=cabinclass, y=survival_rate))+
  geom_bar(fill = "#003399",stat = 'identity')+
  geom_text(aes(label=round(survival_rate,2)), 
            vjust=1.6, color="white", size=3.5)+
  labs(title = "Survival Rates",
       subtitle = "Percentage for each Cabin class",
       x = "Cabin Class", 
       y = "Survival Rate") +
  geom_hline(yintercept=0.38, linetype="dashed", 
             color = "#666666", size=2, 
             show.legend = "0.38")+
  geom_text(aes(8.5,0.40,label = "0.38", vjust =0))

#############################################################
####  Understanding fare level impact########################
#############################################################


hist(titanic$fare)
#fare is higly skewed data set only few paid higher value
#to understand the same let's plot the Boxplot
boxplot(titanic$fare)

#let's remove any fare value exceeding 250$ by carefully 
#analysing the box plot
t1= titanic[!titanic$fare>250,]
#total 16 enteries got deleted 16 represents 1.2% of the dataset

#removing the NA within fare
t1= t1[!is.na(t1$fare),]


av1= aov(t1$fare~t1$survived)
summary(av1)

## F statics critical value for df = 1 & df 12889 = is 3.848#
#hence survival rate is depedent on fare ####################


########## Visual Exploration ###############################

#with the box plot

ggplot(t1, aes(x = survived, y = fare)) + geom_boxplot()+
  labs(title = "Relationship b/w Fare & Survived",
       subtitle = "Relative distribution of Died & Survived",
       x = "Died/Survived", 
       y = "Fare")+
  stat_summary(aes(label = ..y..), 
               fun.y = function(x) round(mean(x), 2), 
               geom = "text", 
               size = 4,
               color = "red")


#with the binning operation

t1$farebin<-cut(t1$fare, c(0,50,100,150,200,250))
t1= t1[!is.na(t1$farebin),]
tab3= table(t1$farebin, t1$survived)
data3= as.data.frame.matrix(tab3)
data3$survival_rate= data3$Survived/(data3$Survived+data3$Died) 
data3 = rownames_to_column(data3, var="FareRange")
data3$FareRange=as.factor(data3$FareRange)
ggplot(data=t1, aes(x=farebin, fill=survived)) + 
  geom_bar(position = 'dodge')+
  labs(title = "Survival of Pessangers",
       subtitle = "Count of Died/Survived based on Fare Range",
       x = "Fare Range", 
       y = "Number of Passengers")+
  scale_fill_manual(values=c("#666666", "#003399"))

#Survival rate comparison


ggplot(data=data3, aes(x=FareRange, y=survival_rate))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(survival_rate,2)), 
            vjust=1.6, color="white", size=3.5)+
  labs(title = "Survival Rates",
       subtitle = "Percentage for given fare range",
       x = "Fare Range", 
       y = "Survival Rate") +
  geom_hline(yintercept=0.38, linetype="dashed", 
             color = "#666666", size=2, 
             show.legend = "0.38")+
  geom_text(aes(1,0.40,label = "0.38", vjust =0))




#############################################################
####  Task 2 Dataset & Visual Understanding##################
#############################################################

unc= read.csv('E:/EBAC/FBA/Assignment 4 Prakash/unc_depts.csv')
str(unc)

#the dataset has 12287 observations & 14 variables 
#discrepency in the data : date is coming out to be as integer
#conversion of date to integer is required

table(unc$status)
class(unc$hiredate)
unc$hiredate= as.Date(as.character(unc$hiredate), format = '%Y%m%d')
Mean= round(mean(unc$totalsal),0)
Median = round(median(unc$totalsal),0)

#histogram for the age and analysis 
ggplot(data=unc, aes(unc$totalsal))+
  geom_histogram(aes(fill=..count..), binwidth=20000)+
labs(title = "Salary distribution",
     subtitle = "Histogram of TotalSal",
     x = "Salary", 
     y = "Count")+
  xlim(c(10000,500000))+
  geom_vline(xintercept=Median,linetype="dashed", size=2, color = "#666666", size=0.5) +
  geom_text(aes(30000,4000,label = "Median 59342", vjust =0))+
  geom_vline(xintercept=Mean,linetype="dashed", size=2, color = "#666666", size=0.5) +
  geom_text(aes(125000,3500,label = "Mean 809378", vjust =0))






###########Plotting age with the salary##########################
options(scipen=999)
ggplot(unc, aes(x=age, y=totalsal))+geom_point()+
  geom_smooth(method="loess", se=F)+
labs(title = "Salary & Age distribution",
     subtitle = "Scatterplot of TotalSal & Age",
     x = "Age", 
     y = "Salary of the Employee")

#let's bin the age for the better understanding
unc$agebin<-cut(unc$age, c(20,40,60,80,100))

agebin_average <- tapply(unc$totalsal, unc$agebin, mean)
agebin_median <- tapply(unc$totalsal, unc$agebin, median)
data5 <- data.frame(agebin_average, agebin_median)
data5 = rownames_to_column(data5, var="agebin")

#mean plot of the age
ggplot(data=data5, aes(x=agebin, y=agebin_average))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(agebin_average,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Average salary",
       subtitle = "For each Age bin",
       x = "Age Bin", 
       y = "Salary") +
  geom_hline(yintercept=Mean, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(1,85000,label = Mean, vjust =0))

#median plot of the age
ggplot(data=data5, aes(x=agebin, y=agebin_median))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(agebin_median,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Median salary",
       subtitle = "For each Age bin",
       x = "Age Bin", 
       y = "Salary") +
  geom_hline(yintercept=Median, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(1,65000,label = Median, vjust =0))


###########Plotting department with the salary##########################

dept_average1 <- tapply(unc$totalsal, unc$dept, mean)
dept_average1 = sort(dept_average1, decreasing= TRUE)
dept_average= head(dept_average1,15)
dept_average= data.frame(dept_average)
dept_average = rownames_to_column(dept_average, var="department")
ggplot(data=dept_average, aes(x=reorder(department, -dept_average), y=dept_average))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(dept_average,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Average Salary",
       subtitle = "For the top paying departments",
       x = "Department", 
       y = "Salary") +
  geom_hline(yintercept=Median, linetype="dashed", 
             color = "#666666", size=2 )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Diverging lollipop chart of the departments

theme_set(theme_bw())
dept_average1= data.frame(dept_average1)
dept_average1$dept_average1 <- round((dept_average1$dept_average1 - mean(dept_average1$dept_average1))/sd(dept_average1$dept_average1), 2)
dept_average1$average_type <- ifelse(dept_average1$dept_average1 < 0, "below", "above")
dept_average1 <- dept_average1[order(dept_average1$dept_average1), ] 
dept_average1= rownames_to_column(dept_average1, var="department")

dept_average2= head(dept_average1,15)
dept_average3= tail(dept_average1,15)

#plotting the head for the salary

ggplot(dept_average2, aes(x=reorder(`department`, -dept_average1), y=dept_average1, label=dept_average1)) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `department`, 
                   yend = dept_average1, 
                   xend = `department`), 
               color = "black") +
  geom_text(color="white", size=3) +
  labs(title="Salary Lollipop Chart", 
       subtitle="Normalized variation from 'salary': for departments",
       x = "Department", 
       y = "Normalised Average") + 
  ylim(-10,10) +
  coord_flip()

#plotting the tail for the salary

ggplot(dept_average3, aes(x=reorder(`department`, -dept_average1), y=dept_average1, label=dept_average1)) + 
  geom_point(stat='identity', fill="black", size=10)  +
  geom_segment(aes(y = 0, 
                   x = `department`, 
                   yend = dept_average1, 
                   xend = `department`), 
               color = "black") +
  geom_text(color="white", size=3) +
  labs(title="Salary Lollipop Chart", 
       subtitle="Normalized variation from 'salary': for departments",
       x = "Department", 
       y = "Normalised Average") + 
  ylim(-10,10) +
  coord_flip()

###########Impact of status with the salary##########################
table(unc$status)
status_mean <- tapply(unc$totalsal, unc$status, mean)
status_median = tapply(unc$totalsal, unc$status, median)
data6 <- data.frame(status_mean, status_median)
data6 = rownames_to_column(data6, var="status")

##########Plotting the average ##########################

ggplot(data=data6, aes(x=reorder(status,-status_mean), y=status_mean))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(status_mean,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Average salary",
       subtitle = "For each Status",
       x = "Status", 
       y = "Salary") +
  geom_hline(yintercept=Mean, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(5,85000,label = Mean, vjust =0))

##########Plotting the median ##########################

ggplot(data=data6, aes(x=reorder(status,-status_median), y=status_median))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(status_median,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Median salary",
       subtitle = "For each Status",
       x = "Status", 
       y = "Salary") +
  geom_hline(yintercept=Median, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(5,65000,label = Median, vjust =0))

########Understanding the impact of year on the employment########
unc$year = substr(unc$hiredate, 0, 4)
unc$year= as.integer(unc$year)
ggplot(data=unc, aes(unc$year))+
  geom_histogram(breaks=seq(1980,2015,by=1), aes(fill=..count..))+
  labs(title = "Recruitment of Employee",
       subtitle = "Histogram of Recuritment for Each year ",
       x = "Year", 
       y = "Count")
  

#############impact of the hiring nature on the salary###########

status_mean <- tapply(unc$totalsal, unc$exempt2, mean)
status_median <- tapply(unc$totalsal, unc$exempt2, median)
data7 <- data.frame(status_mean, status_median)
data7 = rownames_to_column(data7, var="exempt")

#mean plot of the hiring nature
ggplot(data=data7, aes(x=reorder(exempt, -status_mean), y=status_mean))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(status_mean,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Average salary",
       subtitle = "For each Exempt catagory",
       x = "Exempt catagory", 
       y = "Salary") +
  geom_hline(yintercept=Mean, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(3,85000,label = Mean, vjust =0))

#######median plot of the age#######################################
ggplot(data=data7, aes(x=reorder(exempt, -status_mean), y=status_median))+
  geom_bar(fill = "#003399", stat = "identity")+
  geom_text(aes(label=round(status_median,0)), 
            vjust=2.5, color="white", size=4)+
  labs(title = "Median salary",
       subtitle = "For each Exempt catagory",
       x = "Exempt catagory", 
       y = "Salary") +
  geom_hline(yintercept=Median, linetype="dashed", 
             color = "#666666", size=2 )+
  geom_text(aes(3,65000,label = Median, vjust =0))


###### relationship b/w the catagorical variables#############
tab = table(unc$exempt2, unc$status)
addmargins(tab)
#the variables are higly correlated and we can perform chi sq test
chisq.test(unc$exempt2, unc$status)
#X2 value came out to be 24574 which is very high and beyond the cutoff.