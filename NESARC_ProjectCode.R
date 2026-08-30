#Find out if there's a correlation between alcohol and ASPD Traits
#Does frequency of alcohol consumption matter?
data<-NESARC
library(descr)
library(Hmisc)
library(ggplot2)

#label(data$DGSTATUS)<-"Drug Use Status past 12 months"
#label(data$S11AQ7B)<-"All ASPD Traits after Age 15(drugs)"

#data neglecting the NA rows and columns in adults over age 15
sub1<-data[!is.na(data$AGE)&!is.na(data$CONSUMER) & data$AGE>15,]
sub1$S2AQ8A<-as.character(sub1$S2AQ8A)
sub1$s2AQ8A[is.na(sub1$S2AQ8A) & sub1$S2AQ3 !=9]<-11
sub1$S2AQ8A<-as.factor(sub1$S2AQ8A)

sub1$S2AQ8A[sub1$S2AQ8A==9]<-NA 
freq(sub1$S2AQ8A)

sub1$S2AQ8B[sub1$S2AQ8B==99]<-NA
freq(sub1$S2AQ8B)
#Assigning Unknown(9) to missing values(NA)
#sub1$S11AQ6B[sub1$S11AQ6B==9]<-NA
#sub1$S11AQ7B[sub1$S11AQ7B==9]<-NA


#------------------------------------LAB 3----------------------------------
freq(as.ordered(sub1$PARADX2))
freq(as.ordered(sub1$ANTISOCDX2 ))
str(sub1$ANTISOCDX2)
data.plot.ANTISOCDX2<-na.omit(sub1[,c("CONSUMER", "ANTISOCDX2")])
levels(data.plot.ANTISOCDX2$ANTISOCDX2)<-c("No Effect", "Effect")
ggplot(data.plot.ANTISOCDX2) + geom_bar (aes (x=ANTISOCDX2)) +
  xlab("ASPD Traits") + ylab("Frequency")



str(sub1$ANTISOCDX2)
data.plot.ANTISOCDX2<-na.omit(sub1[,c("DGSTATUS", "ANTISOCDX2")])
levels(data.plot.ANTISOCDX2$ANTISOCDX2)<-c("No Effect", "Effect")
ggplot(data.plot.ANTISOCDX2) + geom_bar (aes (x=ANTISOCDX2))+ 
  xlab("ASPD Traits After Substance Use")+ ylab("Substance use")

#----------------------------End of LAB 3--------------------------------------

#----------------------------------LAB 4--------------------------------------------
sub1$ALFREQ[sub1$S2AQ8A==1]<-10
sub1$ALFREQ[sub1$S2AQ8A==2]<-9
sub1$ALFREQ[sub1$S2AQ8A==3]<-8
sub1$ALFREQ[sub1$S2AQ8A==4]<-7
sub1$ALFREQ[sub1$S2AQ8A==5]<-6
sub1$ALFREQ[sub1$S2AQ8A==6]<-5
sub1$ALFREQ[sub1$S2AQ8A==7]<-4
sub1$ALFREQ[sub1$S2AQ8A==8]<-3
sub1$ALFREQ[sub1$S2AQ8A==9]<-2
sub1$ALFREQ[sub1$S2AQ8A==10]<-1


sub1$ALFREQ<-as.factor(sub1$ALFREQ)
# The values for new variable ALFREQ
#1=1-2 days/year
#2=3-6 days/year
#3=7-11 days/year
#4=once a month
#5=2-3 days/month
#6=once a week
#7=2 days/week
#8=3-4 days/week
#9=nearly everyday
#10=everyday


sub1$ALFREQYR[sub1$S2AQ8A==1]<-365
sub1$ALFREQYR[sub1$S2AQ8A==2]<-260
sub1$ALFREQYR[sub1$S2AQ8A==3]<-208
sub1$ALFREQYR[sub1$S2AQ8A==4]<-104
sub1$ALFREQYR[sub1$S2AQ8A==5]<-52
sub1$ALFREQYR[sub1$S2AQ8A==6]<-36
sub1$ALFREQYR[sub1$S2AQ8A==7]<-12
sub1$ALFREQYR[sub1$S2AQ8A==8]<-9
sub1$ALFREQYR[sub1$S2AQ8A==9]<-4.5
sub1$ALFREQYR[sub1$S2AQ8A==10]<-1.5


# values for ALFREQYR(Alcohol Usage Frequency per Year)
#1-2 days/year= 1.5 days/year
#3-6 days/year= 4.5 days/year
#7-11 days/year=9 days/ year
#=once a month=12 days/year
#=2-3 days/month=36 days/year
#=once a week=52 days/year
#=2 days/week= 104 days/year
#=3-4 days/week= 208 days/year
#=nearly everyday = 260 days/year
#=everyday =365 days/year

freq(sub1$ALFREQYR)
table(sub1$ALFREQYR,sub1$S2AQ8A)

#Bottles/ Year estimate?
sub1$BO_YR_EST<-sub1$ALFREQYR*sub1$S2AQ8B
freq(sub1$BO_YR_EST)

sub1[1:100,c("IDNUM","ALFREQYR","S2AQ8B","BO_YR_EST")]

#EXPLANATORY VARIABLE=ALCOHOL
#RESPONSE VARIABLE=ASPD TRAITS
label(data$CONSUMER)<-"Alcohol Status past 12 months"
label(data$ANTISOCDX2)<-"All ASPD Traits"
label(data$S2AQ8A)<-"Frequency of alcohol consumption"
label(data$S2AQ8B)<-"Quantity of alcohol consumed last 12 months"

freq(data$CONSUMER)
freq(data$ANTISOCDX2)
freq(data$S2AQ8A)
freq(data$S2AQ8B)

freq(as.ordered(data$S2AQ8A))
freq(as.ordered(data$S2AQ8B))

freq(as.ordered(sub1$CONSUMER))
freq(as.ordered(sub1$ANTISOCDX2))
freq(as.ordered(sub1$S2AQ8A))
freq(as.ordered(sub1$S2AQ8B))
freq(as.ordered(sub1$AGE))

freq(data$ANTISOCDX2)
freq(sub1$BO_YR_EST)

library(ggplot2)
# To make sure the variable stored in R are of correct types
str(data$ANTISOCDX2)

#creating a temporary subset of data using the function available in ggplot2
data.plot.ANTISOCDX2<-na.omit(sub1[, c("IDNUM","ANTISOCDX2")])

levels(data.plot.ANTISOCDX2)<-c("Current User", "Ex- User", "Abstainer")
ggplot(data=data.plot.ANTISOCDX2)+geom_bar(aes(x=ANTISOCDX2))+
  xlab("ASPD Traits")+ylab("Frequency")



# Syntax for ggplot2
# ggplot(data=data)+geom_bar(aes(x=CategoricalVariableName))
# plotting the bo_yr_est
ggplot(data=sub1)+geom_histogram(aes(x=BO_YR_EST))+
  ylab("Frequency")+xlab("Avg # of Bottles Consumed/Year")+ggtitle("Estimated # Bottles per Year among Adult drinkers over 15")

summary(sub1$BO_YR_EST)
sd(sub1$BO_YR_EST,na.rm=TRUE)

summary(sub1$ANTISOCDX2)
freq(sub1$ANTISOCDX2)

#--------------Starting of bivariate Graphing------------------------------------
freq(as.ordered(sub1$BO_YR_EST))
#ASSUMING A PACK OF ALCOHOL CONTAINS 12 BOTTLES
sub1$PACKSPERYEAR<-sub1$BO_YR_EST/12
freq(as.ordered(sub1$PACKSPERYEAR))


sub1$PACKCATEGORY[sub1$PACKSPERYEAR<=4]<-2
sub1$PACKCATEGORY[sub1$PACKSPERYEAR>4 & sub1$PACKSPERYEAR<=18]<-11
sub1$PACKCATEGORY[sub1$PACKSPERYEAR>18 & sub1$PACKSPERYEAR<=60]<-39
sub1$PACKCATEGORY[sub1$PACKSPERYEAR>60 & sub1$PACKSPERYEAR<=195]<-127
sub1$PACKCATEGORY[sub1$PACKSPERYEAR>195]<-520
freq(as.ordered(sub1$PACKCATEGORY))
sub1$PACKCATEGORY<-as.factor(sub1$PACKCATEGORY)

data.plot.bivariate1<-na.omit(sub1[, c("IDNUM","PACKCATEGORY","ANTISOCDX2")])
data.plot.bivariate1$ANTISOCDX2<-as.numeric(as.character(data.plot.bivariate1$ANTISOCDX2))

#ggplot syntax
#ggplot(data=data.plot.bivariate1)+stat_summary(aes(x=PACKCATEGORY,y=TAB12MDX),fun.y=mean,geom="bar")
ggplot(data=data.plot.bivariate1)+
  stat_summary(aes(x=PACKCATEGORY,y=ANTISOCDX2),fun.y=mean,geom="bar")+
  ylab("Proportion Diagnosed with ASPD")+
  xlab("Packs of Alcohol taken in the past year")+
  ggtitle("ASPD Diagnosis by Alcohol Quantity")


#---------------End of Bivariate Graphing & LAB 4------------------------------------

#---------------------------ANOVA f-test-----------------------------------------
# aov function
# aov (Response variable ~Explanatory variable)
numBO.aov<-aov (BO_YR_EST~GENAXLIFE, data=sub1)
summary(numBO.aov)
#RESULTS gotten in the console from running the code above
#df=degrees of freedom
#Pr(>F)=p-value that corresponds to the f statistics
#f value=f statistics

#My p-value is 8.64e-06 which is less than 0.05 so I'll reject the null hypothesis
#Take the mean of both instances just to be sure
by(sub1$BO_YR_EST,sub1$GENAXLIFE,mean,na.rm=T)

numbo.eth.aov<-aov (BO_YR_EST~ETHRACE2A, data=sub1)
summary(numbo.eth.aov)

# POST HOC TEST to determine whether the more than two groups or categories are indeed different
# there are family of tests
# However, it is important that we do conduct a test

TukeyHSD(numbo.eth.aov)
#----------------------------END OF ANOVA------------------------------------------

#-------------------------CHI SQUARE TEST-------------------------------------------
#AL=Alcohol
AL.chisq<-chisq.test(sub1$ANTISOCDX2, sub1$ALFREQYR)
AL.chisq

AL.chisq$observed
AL.chisq$expected
prop.table(AL.chisq$observed,2)

#POST HOC TEST
#Bonferroni Adjustment
#I have 36 comparisons
#Adjusted p-value = 0.05/36= 0.00138889 / 0.001
source('https://raw.githubusercontent.com/PassionDrivenStatistics/R/master/ChiSquarePostHoc.R')
chisq.post.hoc(AL.chisq$observed, control = "bonferroni", popsInRows = FALSE) [,1:2]

#-------------------------END OF CHI SQUARE TEST-----------------------------------------------------

#---------------------------MODERATION----------------------------------------
str(sub1)                      # Check the structure of the dataset
summary(sub1$S2AQ8B)          # Summarize the column S3AQ3C1
unique(sub1$S2AQ8B)           # View unique values in S3AQ3C1
table(is.na(sub1$S2AQ8B))     # Check for missing values in S3AQ3C1

sub1$S2AQ8B <- as.numeric(sub1$S2AQ8B)
if (!"S2AQ8B" %in% names(sub1)) stop("S2AQ8B does not exist in sub1")

# Quantity of Drinks taken per day= ALDAY
# Initialize ALDAY with NA
sub1$ALDAY <- NA  

# Assign values to ALDAY based on ranges of S2AQ8B
sub1$ALDAY[sub1$S2AQ8B >= 1 & sub1$S2AQ8B <= 5] <- 3
sub1$ALDAY[sub1$S2AQ8B >= 6 & sub1$S2AQ8B <= 10] <- 8
sub1$ALDAY[sub1$S2AQ8B >= 11 & sub1$S2AQ8B <= 15] <- 13
sub1$ALDAY[sub1$S2AQ8B >= 16 & sub1$S2AQ8B <= 20] <- 18
sub1$ALDAY[sub1$S2AQ8B >= 21] <- 37

# Optionally, set values to 0 where S2AQ8B does not meet any range condition
sub1$ALDAY[is.na(sub1$S2AQ8B) | sub1$S2AQ8B == 0] <- 0  

# Verify the results
table(sub1$ALDAY, useNA = "ifany")  # Frequency table for ALDAY
freq(sub1$ALDAY)

# values for variable ALDAY
#3 =1 to 5 drinks/day
#8 =6 to 10 drinks/day
#13=11-15 drinks/day
#18=16-20 drinks/day
#37=21-37 drinks/day
#0 =non-daily drinking

AL14.chisq<-chisq.test(sub1$ANTISOCDX2,sub1$ALDAY)
AL14.chisq
AL14.chisq$observed
AL14.chisq$expected
prop.table(AL14.chisq$observed,2)

#ALUSCAN=Alcohol US 
data.plot.ALUSCAN<-sub1[,c("IDNUM", "ANTISOCDX2","ALDAY")]
data.plotALUSCAN<-na.omit(data.plot.ALUSCAN)

data.plot.ALUSCAN$ANTISOCDX2<-as.numeric(as.character(data.plot.ALUSCAN$ANTISOCDX2))
ggplot(data=data.plot.ALUSCAN)+
  stat_summary(aes(x=ALDAY,y=ANTISOCDX2),fun.y=mean, geom="bar")

by(sub1,sub1$MAJORDEPLIFE,function(x)
  list(
    chisq.test(x$ANTISOCDX2,x$ALDAY),#Runs chi square test btw alcohol & ASPD 
    chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,#Creates a crosstab litter table
    prop.table(chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,2)) #calculates column percentages
)

# Create the data for the"No Depression" chart
A <- c(0.01, 0.022, 0.065, 0.107, 0.259,0.22)
B <- c(0,3,8,13,18,37)

# Plot the bar chart
barplot(A, names.arg = B, xlab ="Alcohol Per Day",
        ylab ="ASPD Traits", col ="PINK",
        main ="No Depression")

# Create the data for the "Depression" chart
C <- c(0.066,0.078,0.20,0.46,0.368,0.71)
D <- c(0,3,8,13,18,37)

# Plot the bar chart
barplot(C, names.arg = D, xlab ="Alcohol Per Day",
        ylab ="ASPD Traits", col ="PINK",
        main ="Depression")


by(sub1,sub1$PARADX2,function(x)
  list(
    chisq.test(x$ANTISOCDX2,x$ALDAY),#Runs chi square test btw alcohol & ASPD 
    chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,#Creates a crosstab litter table
    prop.table(chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,2)) #calculates column percentages
)

by(sub1,sub1$TAB12MDX,function(x)
  list(
    chisq.test(x$ANTISOCDX2,x$ALDAY),#Runs chi square test btw alcohol & ASPD 
    chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,#Creates a crosstab litter table
    prop.table(chisq.test(x$ANTISOCDX2,x$ALDAY)$observed,2)) #calculates column percentages
)

#---------------------------------END OF MODERATION--------------------------------------

#----------------------------------LINEAR REGRESSION----------------------------------------
#glm
#Does ASPD correlate with PPD(Paranoid Personality Disorder) at any point

sub1$ANTISOCDX2
table(sub1$ANTISOCDX2)
freq(sub1$ANTISOCDX2)

m1ND<-glm(ANTISOCDX2~PARADX2, data=sub1, family="binomial")
summary(m1ND)

exp(m1ND$coefficients)
##The odds of having ASPD is 7.6 times higher among people with PPD than it is in people without
exp(confint(m1ND))
#confint==Confidence Interval
#Has a confidence interval of 6.69 to 8.67
#We can be 66% sure that the confidence interval would contain the value of the odds ratio in the actual population

m2ND<-glm(ANTISOCDX2~PARADX2+MAJORDEPLIFE, data=sub1, family="binomial")
summary(m2ND)

exp(m2ND$coefficients)
exp(confint(m2ND))
#Among alcohol consumers(DAILY),the odds of having ASPD among those WITH PPD is 4.8 times higher than those without after controlling for major depression
#The odds for ASPD is 3.1 times higher among those WITH depression than those without after controlling for PPD


m5ND<-glm(ANTISOCDX2~TAB12MDX,data = sub1, family = "binomial")
summary(m5ND)
exp(m5ND$coefficients)
exp(confint(m5ND))

m6ND<-glm(ANTISOCDX2~ALFREQ,data=sub1,family = "binomial")
summary(m6ND)

m3ND<-glm(ANTISOCDX2~ALDAY, data=sub1, family="binomial")
summary(m3ND)
exp(m3ND$coefficients)
exp(confint(m3ND))

m4ND<-glm(ANTISOCDX2~ CONSUMER,data=sub1,family="binomial")
summary(m4ND)
exp(m4ND$coefficients)
exp(confint(m4ND))

