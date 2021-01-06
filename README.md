# PSI-Independent-Assignment
c15339871


Student Number: C15339871
Name: Erika Secillano
Programme Code: TU060-1
Section 1 - Research Question(s)
In this section we will be identifying the research question(s) for this assignment and dataset. We will be using the Portugal bank dataset. The data set that we will be working on is a well-known data set. We will be acquiring it in the UC Irvine Machine Learning Repository (UCI ML Repository), the data was collected from 2018 to 2013, which during this time includes the global financial crisis in the year 2008. The student must identify the customers who will most likely subscribe to a term deposit account that will be based on their previous marketing campaigns. It is important to investigate this as it will allow for the business to see how many of their customers will subscribe, and the business can change their approach depending on the outcome. If most of their customers subscribe this shows that their marketing strategy has been successful, if not the business must investigate the issue and re-think their method of marketing. Our research question will be, “Can a customer’s marital, education and age affect their choice to subscribe to a long-term deposit and is there a differential effect on customer’s with a loan such as housing?”. With our research question stated, in the next part we will look and explore our datasets.
Section 2 - Dataset
In this part, the student will investigate the data that was used for the assignment and document any characteristics and other information that they can conclude. Analyse any data insights that were discovered and present the data preparation and what decisions were made. The data were 2 sets of customers. In the previous campaign there were 5625 and there are 35563 who were contacted the first time. This part is very important as we must have good quality data to work on, as this will result in results/accuracy. From looking into the attribute information in UCI ML Repository here are what we get:

Table 1: Statistical Measurement Types of the data and their description
Attribute
Description
Data Type
age
age
numeric
job
type of job
categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown'
marital 
marital status:bank client data
categorical: 'divorced','married','single','unknown'; note: 'divorced' means divorced or widowed
education
education:bank client data
categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown'
default
has credit in default?:bank client data
categorical: 'no','yes','unknown'
housing
has housing loan?:bank client data
categorical: 'no','yes','unknown'
loan
has personal loan?:bank client data
categorical: 'no','yes','unknown'
contact
contact communication type:related with the last contact of the current campaign
categorical: 'cellular','telephone'
month
last contact month of year:related with the last contact of the current campaign
categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec'
day_of_week
last contact day of the week:related with the last contact of the current campaign
categorical: 'mon','tue','wed','thu','fri'
duration
last contact duration, in seconds:related with the last contact of the current campaign
(numeric). Important note: this attribute highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not known before a call is performed. Also, after the end of the call y is obviously known. Thus, this input should only be included for benchmark purposes and should be discarded if the intention is to have a realistic predictive model.
 campaign
number of contacts performed during this campaign and for this client:other attributes
numeric, includes last contact
pdays
number of days that passed by after the client was last contacted from a previous campaign:other attributes
numeric; 999 means client was not previously contacted
previous
number of contacts performed before this campaign and for this client:other attributes
numeric
poutcome
outcome of the previous marketing campaign:other attributes
categorical: 'failure','nonexistent','success'
emp.var.rate
employment variation rate - quarterly indicator
numeric
cons.price.idx
consumer price index - monthly indicator  
numeric
cons.conf.idx
consumer confidence index - monthly indicator
numeric
euribor3m
euribor 3 month rate - daily indicator
numeric
nr.employed
number of employees - quarterly indicator
numeric
y
has the client subscribed a term deposit? Output variable (desired target)
binary: 'yes','no'

Descriptive statistics and visuals and missing values
This data was acquired through telemarketing phone calls to sell long term deposits. There are inbound and outbound types of selling, outbound meaning that the clients are called and if the clients call the agents and they are asked to subscribe. We read in the data using the code below. Age has a big range between minimum of 17 and maximum of 98 years old!

#Read dataset
bank_full = read.csv(file = 'bank-additional-full.csv', sep =';', header = TRUE)

#Size of dataset
nrow(bank_full)
#[1] 41188

ncol(bank_full)
#[1] 20

#view statistical summary
summary(bank_full)

#sample 
head(bank_full)
Summary Result

Sample Result


The target variable which is variable ‘y’ will be changed to binary using the following code. The outcome will be binary which is the variable target y in the data. 

#Replace the target variables yes = 1, no = 0
bank_full$y<-ifelse(bank_full$y =='yes', 1,0)
bank_full$y<-as.factor(bank_full$y)

In the code below we plotted the difference between the yes (1) and no (0). It is seen that majority of the clients said no and only a few subscribed to the long term deposit.

#Plot the difference of Y (outcome)
counts <- table(bank_full$y)

barplot(counts,col=c("darkblue","red"),legend = rownames(counts), main = "Term Deposit")





In our numerical, we will look into a correlation matrix. This can be used to easily check the correlation between two numerical features. Looking into the results, the highest correlation is 0.97 with euribor3m and emp.var.rate. The other high correlation variables are emp.var.rate, euribor3m, nr.employed and cons.price.idx.

All our variables do not have any missing values as shown below in our plot. The data didn’t have any missing values, therefore we do not need to drop any observations or impute values.

#Check for duplicate rows
sum(duplicated(bank_full))
#[1] 1784

sum(!complete.cases(bank_full))
#[1] 0

#Plot the missing values
aggr_plot <- aggr(bank_full, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(bank_full), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))




#get numerical
new_df <- bank_full[sapply(bank_full,is.numeric)]
#Plot correlation plot
library(corrplot)
corrplot.mixed(cor(new_df),
               lower = "number", 
               upper = "circle",
               tl.col = "black")
Figure 1: Numerical Variables

In exploring our categorical variables with Job we see that the customer with an admin job has the highest subscribing in term deposit. Next is Marital, customers are mostly married and married customers have the most yes outcome compared to the rest. In Default, No has the biggest yes. In housing, the majority have a housing loan and people that have a housing loan tend to have to say yes to having a long term deposit.In poutcome, we can see that most of the customers have non-existent previous marketing campaigns. We can also see that customers who had a successful outcome from the previous campaign, majority of customers did subscribe for a term deposit. On days of the week, we can see that it has a similar distribution. 

#rename y to Target
colnames(bank_full)[colnames(bank_full) == 'y'] <- 'target'

#plot the categorical variables change job to the relevant column to plot
ggplot(bank_full %>% count(job, target) %>%    
         mutate(pct=n/sum(n),               
                ypos = cumsum(n) - 0.5*n), 
       aes(job, n, fill=target)) +
  geom_bar(stat="identity")



JOB

DAY OF WEEK

HOUSING

MARITAL

POUTCOME

DEFAULT

Figure 2: Categorical Variables
Age

##Check age distribution
summary(bank_full$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.00   32.00   38.00   40.02   47.00   98.00 

#Plot the age distribution
gg = ggplot (bank_full) 
age_dis = gg + geom_histogram(aes(x=age),color="black", fill="white", binwidth = 5) +
  ggtitle('Age Distribution') +
  ylab('Count') +
  xlab('Age') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")

#Plot the age through boxplot
age_box_plot = gg + geom_boxplot(aes(x='', y=age)) +
  ggtitle('Age Boxplot') +
  ylab('Age')

##Plot Age distribution X Marital Status
age_marital <- ggplot(bank_full, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  ggtitle("Age Distribution by Marital Status")

#Get the mean age
mean_age <- bank_full %>% group_by(target) %>% summarise(grp.mean=mean(age))

#Plot age and mean
age_sub <- ggplot (bank_full, aes(x=age)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 5) +
  facet_grid(cols=vars(target)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age') +
  scale_x_continuous(breaks = seq(0,100,5)) +
  geom_vline(data=mean_age, aes(xintercept=grp.mean), color="red", linetype="dashed")

#Print all the plots
grid.arrange(age_dis, age_box_plot,age_marital,age_sub, ncol = 2, nrow = 2)

In looking further into the Age variable, the data was plotted. In the summary we see that most of the clients are from 33 - 48 that is the 1st Quartile and 3rd Quartile. The mean shows that the age is 41 and this can be clearly seen in the histogram with the red line. The boxplot shows that the outliers are above the age of 65. In the age distribution by marital status. We can see that most of the clients are married or divorced. There is a large difference after the age of 60 with the most marital status divorced and married. The single clients decay as they grow older and get married. In the final plot, it is seen that most of the clients subscribe between ages 25 - 45. 

Education and Subscription

#Education X subscription
ggplot(data = bank_full, aes(x=education, fill=target)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

We graph the education with the subscription. We can see from the results that people with higher education tend to subscribe more to the term deposit. The University degree and the Highschool are the ones that subscribe the most, especially with the University degree.

Subscription based on Campaign
#Subscription based on the campaign
ggplot(data=bank_full, aes(x=campaign, fill=target))+
  geom_histogram()+
  ggtitle("Subscription based during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))


In the results, it is seen that after the 9th contact during the campaign, there were no more subscriptions done by the customers. This suggests that there should be a limit of contacts during a campaign. They can set it to 3 contacts as this is the highest contact that the customers said yes to subscribe to the term deposit.

Duration

##Duration
range(bank_full$duration)
#[1]    0 4918

summary(bank_full$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   102.0   180.0   258.3   319.0  4918.0 

bank_full %>% select(duration) %>% arrange(desc(duration)) %>% head
# duration
# 1     4918
# 2     4199
# 3     3785
# 4     3643
# 5     3631
# 6     3509

##get the mean duration
mean_duration<- bank_full %>% group_by(target) %>% summarise(grp2.mean=mean(duration))

##plot the duration
duration_count <- ggplot(bank_full, aes(x=duration, fill = target)) +
  geom_histogram(binwidth = 2) +
  facet_grid(cols = vars(target)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400)) + geom_vline(data = mean_duration, aes(xintercept = grp2.mean), color = "red", linetype = "dashed")

##duration x camp
duration_camp <- bank_full %>% filter(campaign < 63) %>% 
  ggplot(aes(campaign, duration)) +
  geom_point() +
  facet_grid(cols = vars(target))

#print the plots
grid.arrange(duration_count,duration_camp, ncol = 2)
For the duration the range is very big, 0 - 4918. The duration that’s the most common is 1. The median is 180. 
Representativeness
The sample taken must be representative, meaning that we seek to accurately mirror the characteristics of the population. Although there are a few factors that influence the accuracy of a sample’s ability to represent a population such as randomness and size. A sample is considered as biased if it is not representative and we must avoid such issues. For our sample, in regard to the representativeness, it is not representative of all of the people that are contacted through marketing campaigns from different banking institutions. This is only from a single bank and their customers, which is most likely not all of their customers.
In the next section we will be working on our Models.
Section 3 - Results 
In this section we are going to present the results of our tests and models. 
Section 3.1 - Statistical Evidence
In this section we will be doing a correlation and difference test that we will need to use for the models. We will be looking into summary statistics, histograms and Q-Q Plots. But first we will remove all the unknown variables as we will need to compare within 2 groups.
#Replace all unknown with NA
bank_full[bank_full=="unknown"] <- NA

#remove na
bank_full <- bank_full[complete.cases(bank_full),]

Assessing Normality of Variables
We will assess the normality in this part of the assignment so that we are able to work on our model. In our results we are only interested in age which is the first result. We can see that that age is skewed on the left.

library(plyr)
library(psych)

#plot and make numeric
multi.hist(bank_full[,sapply(bank_full, is.numeric)])

> pastecs::stat.desc(bank_full$age, basic=F)

      median         mean      SE.mean CI.mean.0.95          var      std.dev 
 37.00000000  39.03001181   0.05918126   0.11599774 106.78182633  10.33352923 
    coef.var 
  0.26475855 



#QQplot age
qqnorm(bank_full$age); qqline(bank_full$age,col ="steelblue", lwd = 2)

Homogeneity of Variance
We use Homogeneity of variance to see the pattern in variance of the variable that we are working on around the mean of each group. If homogeneity is present the t-test power is increased. Lavene test is used to test this. In our results below we see that the result of our levene test is not statistically significant as PR(>F) is > 0.05. Even with our results of boxplot it looks like the median is the same.The variance is not statistically significantly different thus we assume homogeneity. Looking into the boxplot we can see that most customers aged between 30 - 45 have a housing loan. After age 65 there are a lot of outliers.

#Homogeneity of Variance
bank_full %>% gather(age, key = 'var', value = 'value') %>%
  ggplot(aes(x = var, y = value, fill = housing)) +
  geom_boxplot() +
  theme_bw()

> car::leveneTest(age ~ housing, data=bank_full)

Levene's Test for Homogeneity of Variance (center = median)
         Df F value Pr(>F)
group     1  1.9359 0.1641
      30486  

> car::leveneTest(age ~ housing, data=bank_full, center = 'mean')

Levene's Test for Homogeneity of Variance (center = "mean")
         Df F value  Pr(>F)  
group     1  3.3772 0.06611 .
      30486                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Run tests
We will be using the parametric independent samples t-test to evaluate if there is a difference between the group. The variance is equal as from our previous Lavene test were no statistical difference in variance was discovered. 
> stats::t.test(age~housing,var.equal=TRUE,data=bank_full)

	Two Sample t-test

data:  age by housing
t = -0.81333, df = 30486, p-value = 0.416
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.3294226  0.1362076
sample estimates:
 mean in group no mean in group yes 
         38.97766          39.07427 

> psych::describeBy(bank_full$age, bank_full$housing, mat=TRUE)
    item group1 vars     n     mean       sd median  trimmed    mad min max range      skew kurtosis         se
X11    1     no    1 13967 38.97766 10.26829     37 38.05817 8.8956  18  95    77 0.9910800 1.295258 0.08688538
X12    2    yes    1 16521 39.07427 10.38847     37 38.16343 8.8956  17  91    74 0.9707474 1.196006 0.08082268

Res <- stats::t.test(age~housing,var.equal=TRUE,data=bank_full)
 #Calculate Cohen's d
 #artithmetically
 effcd=round((2*res$statistic)/sqrt(res$parameter),2)
 
 #Using function from effectsize package
 effectsize::t_to_d(t = res$statistic, res$parameter)
        d |        95% CI
-------------------------
-9.32e-03 | [-0.03, 0.01]

 effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
 effes
t 
0 

Result
An independent-samples t-test was conducted to compare housing loans for different ages. No significant difference in the scores for housing loan was found (M=37, SD=10.27 for respondents who have no housing loan, M=37, SD=10.39 for respondents who have a housing loan), (t(30486)=  -0.81333, p = 0.416. Cohen's d also indicated a small effect size (-0.16). No effect size was also indicated by the eta squared value (0). 
In the next part we will be using logistic regression.
Section 3.2 – Logistic Regression Model  
Logistic regression tries to classify observation, it does this by estimating if the probability of an observation in a certain category or not. In logistic regression we will be using categorical variables as continuous variables are for Linear regression. For our hypothesis, ‘Marital Status and education of a Customer will have an impact on the target variable’. We will look into the marital status and an additional predictor such as age. Two predictors are needed per model. We will be using a binary logistic regression and our models as our target variable has a 0 (no) or 1 (yes) outcome. The output that we get from logistic regression is a probability that the given input belongs to a category. Our outcome variable is the probability that the customers will not subscribe to the long term deposit. We will build a base mode for the first model and in the next model we are able to remove or add more predictors.
Run the model
#LOGISTIC MODEL
logmodel1 <- glm(target ~ marital + education, data = bank_full, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel1)



In our results we see that our overall model is statistically significant with the PR(>chisq) = <0.05. PR(>chisq) columns show the two-tailed p-values testing the null-hypothesis. We see that being divorced, highschool, professional course and 6y are statistically significant with their PR(>chisq) = <0.05. The rest is not statistically significant as they are >0.05. When we look into the estimate column there is a negative change in the outcome in the predictor divorced,6y,9y, highschool and professional course. With the rest having a positive change. The standard error of the coefficient estimates. This represents the accuracy of the coefficients. When the standard error is larger, the less confident we are about the estimate. The largest standard error that we see is .68 with the illiterate column. The rest has < 1 standard error as well.
Chi-square
> #Chi-square plus significance
> lmtest::lrtest(logmodel1)
Likelihood ratio test

Likelihood ratio test

Model 1: target ~ marital + education
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   9 -11500                         
2   1 -11580 -8 160.25  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Odds ratio and probability
## odds ratios 
cbind(Estimate=round(coef(logmodel1),4),
      OR=round(exp(coef(logmodel1)),4))




The odds ratio result(OR)  is an indication of the change in odds result from a unit change in the predictor. The OR in divorce is 0.15 to 1, increase in married to 1.04 to 1, increase in single 1.3 to 1, decrease in 6y 0.68 to 1, decrease in 9y 0.6 to 1, increase in highschool 0.82 to 1 increase in illiterate 2.44 to 1, decrease in professional course 0.8607 to, increase in university degree 0.03 to 1. 
#1. Probability of answering yes when divorced 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0)
#Probability of answering yes when married 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1)
#Probability of answering yes when single 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2)
#Probability of answering yes when unknown
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*3)

##with the column education
#3.Probability of answering yes when divorced 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*1+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*2+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*3+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*4+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*5+coef(logmodel1)[3]*0)
#3.Probability of answering yes when married  
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*1+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*2+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*3+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*4+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*5+coef(logmodel1)[3]*0)
#Probability of answering yes when single 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*1+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*2+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*3+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*4+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*5+coef(logmodel1)[3]*0)

###with neither 
#3.Probability of answering yes when divorced 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*1)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*2)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*3)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*4)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*0 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*5)
#3.Probability of answering yes when married 
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*1)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*2)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*3)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*4)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*1 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*5)
#Probability of answering yes when single 0
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*0)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*1)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*2)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*3)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*4)
arm::invlogit(coef(logmodel1)[1]+ coef(logmodel1)[2]*2 +coef(logmodel1)[3]*0+coef(logmodel1)[3]*5)

Case
Probability
Case
Probability
Case
Probability
Divorced
0.1295686 
Divorced + 6y
0.1295686
Divorced - 6y
0.1295686
Married
0.1339135 
Divorced + 9y
0.1627865
Divorced - 9y
0.1627865
Single
 0.138381 
Divorced + highschool
0.2025391 
Divorced - highschool
0.2025391 




Divorced + illiterate
0.2491108 
Divorced - illiterate
0.2491108 




Divorced + professional course
0.3023313 
Divorced - professional course
0.3023313 




Divorced + University Degree
0.3614488
Divorced - University Degree
0.3614488




Married + 6y
0.1339135 
Married - 6y
0.1339135 




Married + 9y
0.1680304 
Married - 9y
0.1680304 




Married + highschool
0.2087442 
Married - highschool
0.2087442




Married + illiterate
0.2562842 
Married - illiterate
0.3104037




Married + professional course
0.3702619  
Married - professional course
0.3702619 




Married + University Degree
0.434392 
Married - University Degree
0.434392




Single + 6y
0.138381 
Single - 6y
0.138381 




Single + 9y
 0.1734082
Single - 9y
 0.1734082




Single + highschool
0.2150881
Single - highschool
0.2150881




Single + illiterate
0.2635916
Single - illiterate
0.2635916




Single + professional course
 0.3185932
Single - professional course
 0.3185932




Single + University Degree
0.3791624 
Single - University Degree
0.3791624 
When looking into the probability we see that when we include and exclude the education. The probability is the same. When looking into the probability of marital status, there is a probability of .13, for divorced, married is .13, single is .14 and unknown is .14. In the table where education is added the highest probability is .43 with Married + University Degree, followed by single + University degree .38. It does seem odd that both inclusion and exclusion of education results are the same. 
Goodness of fit of the model
In logistic regression, the omnibus test is used to check if the model has an improvement with the baseline mode (the sum of frequency of occurrence in each category). Chi square test is used for the difference compared to the baseline and the model that was created. This shows that our models are statistically significant PR(>chisq) = <0.5. This states that the difference has a better predictor than a baseline model. We already have seen that this is statistically significant from the previous runs. 
#Chi-square plus significance
lmtest::lrtest(logmodel1)

Model 1: target ~ marital + education
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   9 -11500                         
2   1 -11580 -8 160.25  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Usefulness
Pseudo R squared is used to show how the model is useful. CoxSnell R2 and Nagelkerke R2 are pseudo R2 statistics that we are able to generate. R2 is a measure of how much in our outcome variable was explained by the variable predictor. Our results show that the predictors of marital and education is explained between 0.52 and 1 if the customers will agree to the long term deposit or not. 
#Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")
# CoxSnell 
# 0.005242304 

DescTools::PseudoR2(logmodel1, which="Nagelkerke")
# Nagelkerke 
# 0.009850854 

Sensitivity analysis and summary stats
We use sensitivity and specificity measurements to further understand our model. Specificity is the number of true negatives (TN) that were correctly identified which are negatives that are predicted negative andS sensitivity is the number of true positives (TP) which are positives that are predicted as positives. In the ROC curve we see that the TP is 72.5% and TN is 37.3%. The values that were positively predicted are the ones that the model has identified correctly which is 9.7% and for negative 85.7%.  To classify our performance we use the ROC curve and looking into the Area Under Curve we get .56 which is weak.

#Output the marital and education and ROC plot
library(Epi)#ROC Curve
install.packages("Epi")
ROC(form=target ~ marital+education, data=bank_full,plot="ROC")

Assumption of linearity of independent variables
#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
generalhoslem::logitgof(bank_full$target,fitted(logmodel1))

Hosmer and Lemeshow test (binary model)

data:  bank_full$target, fitted(logmodel1)
X-squared = 14.036, df = 7, p-value = 0.05054
Since we have two predictors variables we will need to check for collinearity using a Hosmer-Lemeshow test. We see that our results are not statistically significant with P > 0.05. According to Tarling (2008), (VIF <2.5,Tolerance >0.4). Comparing it with the results that we have received. Our results are within acceptable levels, suggesting that there is a possibility of collinearity. VIF show’s that we only have little correlation. We use the Hosmer-Lemeshow test in the assumption of linearity of independent variables and log odds.
#Collinearity
vifmodel<-car::vif(logmodel1)
vifmodel

              GVIF Df GVIF^(1/(2*Df))
marital   1.039695  2        1.009779
education 1.039695  6        1.003249
#Tolerance
1/vifmodel

               GVIF        Df GVIF^(1/(2*Df))
marital   0.9618207 0.5000000       0.9903154
education 0.9618207 0.1666667       0.9967613



Reporting
A Logistic regression was built using binary outcomes to test our hypothesis that Marital status and education of customers has an impact for a long term loan that is our target / outcome variable that is 1 = yes and 0 = no. Marital status and education was used as the two predictors.  In marital status there were 3 levels such as divorced, married and single, whilst education had 6 levels such as 6y, 9y, highschool, illiterate, professional course and university degree. Our summary results show that divorced, 6y, highschool and professional course were statistically significant with PR(>|z|) < 0.5. The rest of the variables were not statistically significant with PR(>|z|) > 0.5. The OR in divorce is 0.15 to 1, increase in married to 1.04 to 1, increase in single 1.3 to 1. The probability is .13, .13 and .14 respectively. Our R2 = 0.52 Cox-Snell and 1 Nagelkerke with  PR(>Chisq) = <0.05 which are quite significant results. Examination for multicollinearity showed that the tolerance and variance influence factor measures were within acceptable ranges (tolerance <0.4, VIF <2.5) as outlined in Tarling (2008). The Hosmer Lemeshow goodness of fit statistic did not not indicate issues with the assumption of linearity between the dependent variables and the log odds of the model, our (χ2(n=8) = 160.25, P = <0.05).



Probability
Divorce
0.1295686 
Married
0.1339135 
single
 0.138381 

Section 3.3 – Logistic Regression Model Extended
In this section we will extend our logistic regression model and we will be doing a differential effect on the predictor housing and age. We will be using a  We have already done the test in previous section 3.1 and our results show that there was a difference. ,’Age and housing loan of a Customer will have an impact on the target variable’.
Run the model

In our results we see that our overall model is statistically significant with the PR(>chisq) = <0.05. PR(>chisq) columns show the two-tailed p-values testing the null-hypothesis. We see the housing no and age has are statistically significant with their PR(>chisq) = <0.05. Only housing yes has PR(>chisq) = > 0.05. Looking in the estimate column there is a negative impact on the housing no and the standard error indicated the accuracy of the coefficients and housing no has an accuracy of 0.068803. Housing has a small positive impact of 0.06 with an accuracy of 0.035. Age has a small positive impact of 0.013 with an accuracy of 0.015. 
Chi-square
Likelihood ratio test

Model 1: target ~ housing + age
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   3 -11544                         
2   1 -11580 -2 72.987  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Odds ratio and probability

The odds ratio result(OR)  is an indication of the change in odds result from a unit change in the predictor. The OR in housing no is 0.0822 to 1, with an increase in housing yes 1.0614 to 1 and decrease in age with 1.0136 to 1.
# Probability of answering yes when the loan is no 
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0)

#Probability of answering yes when the loan is no
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1)

#Probability of answering yes when the loan is no and age
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

#Probability of answering yes when the loan is yes and age
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

Case
Probability
Case
Probability
Housing Loan NO
0.07596811
Housing loan NO + age
0.07692105
Housing Loan YES
0.08025821
Housing loan YES + age
0.08126024 
When looking into the probability we see that when we include the age. The probability of yes in housing loan no is 0.075%, the probability of yes in housing loan yes is bigger with 0.08%. Looking into the probability of yes in housing loan no and age is 0.077% and the probability of yes in housing loan no and age is 0.081.
Goodness of fit of the model
In logistic regression, the omnibus test is used to check if the model has an improvement with the baseline mode (the sum of frequency of occurrence in each category). Chi square test is used for the difference compared to the baseline and the model that was created. This shows that our models are statistically significant PR(>chisq) = <0.5, this states that the difference has a better predictor than a baseline model. We already have seen that this is statistically significant from the previous runs. 
Likelihood ratio test

Model 1: target ~ housing + age
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   3 -11544                         
2   1 -11580 -2 72.987  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Usefulness
Pseudo R squared is used to show how the model is useful. CoxSnell R2 and Nagelkerke R2 are pseudo R2 statistics that we are able to generate. R2 is a measure of how much in our outcome variable was explained by the variable predictor. Our results show that the predictors of age and housing are explained between 0.24 and .44 if the customers will agree to the long term deposit or not. 
# CoxSnell 
# 0.002391101  

# Nagelkerke 
# 0.004493135

Sensitivity analysis and summary stats
We use sensitivity and specificity measurements to further understand our model. Specificity is the number of true negatives (TN) that were correctly identified which are negatives that are predicted negative andS sensitivity is the number of true positives (TP) which are positives that are predicted as positives. In the ROC curve we see that the TP is 15.2% and TN is 93.1%. The values that were positively predicted are the ones that the model has identified correctly which is 11.7% and for negative 75.8%. To classify our performance we use the ROC curve and looking into the Area Under Curve we get .506 which is weak.


Assumption of linearity of independent variables
Hosmer and Lemeshow test (binary model)

data:  bank_full$target, fitted(logmodel2)
X-squared = 347.35, df = 8, p-value < 2.2e-16
Since we have two predictors variables we will need to check for collinearity using a Hosmer-Lemeshow test. We see that our results are statistically significant with P < 0.05. According to Tarling (2008), (VIF <2.5,Tolerance >0.4). Comparing it with the results that we have received. Our results are within acceptable levels, suggesting that there is a possibility of collinearity. VIF show’s that we only have little correlation.
#Collinearity
 housing      age 
1.000021 1.000021 
#Tolerance
  housing       age 
0.9999792 0.9999792 

Reporting
A Logistic regression was built using binary outcomes to test our hypothesis that Marital status and education of customers has an impact for a long term loan that is our target / outcome variable that is 1 = yes and 0 = no. Age and housing was used as the two predictors. Age is a continuous variable and in housing there were 2 levels with yes having a housing loan and no having no housing loan within the bank. Our summary results shows that housing no and age had a significant effect with their PR(>|z|) < 0.05. Only housing no was PR(>|z|) > 0.05. The OR in housing no was 0.0822 to 1, housing yes is 1 to 1 and age is 1 to 1. The probability is 0.076 for Housing loan NO in which customers have said yes to the long term deposit and 0.08 for housing loan yes in which customers have said yes to the long term deposit. These results are quite low. Our R2 = 0.24 Cox-Snell and 0.44 Nagelkerke with  PR(>Chisq) = <0.05 which are quite significant results. Examination for multicollinearity showed that the tolerance and variance influence factor measures were within acceptable ranges (tolerance <0.4, VIF <2.5) as outlined in Tarling (2008). The Hosmer Lemeshow goodness of fit statistic did not not indicate issues with the assumption of linearity between the dependent variables and the log odds of the model, our (χ2(n=8) = 72.987, P = <0.05).
Section 3. 4 Summary Comparison
In this section we will be comparing the two models that we have done.
Difference in fit and usefulness


FIT
USEFULNESS
MODEL 1
Model 1: target ~ marital + education
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   9 -11500                         
2   1 -11580 -8 160.25  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# CoxSnell 
# 0.005242304 

# Nagelkerke 
# 0.009850854 
MODEL 2
Likelihood ratio test

Model 1: target ~ housing + age
Model 2: target ~ 1
  #Df LogLik Df  Chisq Pr(>Chisq)    
1   3 -11544                         
2   1 -11580 -2 72.987  < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# CoxSnell 
# 0.002391101  

# Nagelkerke 
# 0.004493135

Model 1 looks into the predictors marital and education which are both categorical data. Model 2 looks into the predictor housing and age. Housing being a categorical data and age being a continuous data. They  both have the same target variable which is ‘target’ and was converted to binary in earlier steps. In the variable ‘target’ 1 means yes that the customers subscribed to the long term deposit or 0 for no. For both of the models we see that their PR(>Chisq) is < 0.05, which states that our results are statistically significant. Looking into CoxSnell (R2) for Model 1 it’s 0.052 and Model 2 0.023, ideally it should be high to show correlation. =>0.7 is considered as high correlation and =< 0.4 shows a low correlation. Our results tell us that there is a low correlation. Looking into Nagelkerke for Model 1 it’s 0.099 and Model 2 0.045. The same case goes for Nagelkerke, it shows that our correlations are low and not ideal. 
Stability or otherwise of predictors across model








MODEL 1




MODEL 2






How models meet assumptions
When choosing to analyse with binomial logistic regression, the outcome probability is 0 or 1 based on one or more independent variables. We will ask ourselves if our model has met the 4 assumptions needed.
Assumption 1 - “Dependent variables should be measured  on a dichotomous scale.”
In this assumption, our target variable is the dependent variable. Yes = 1 or No = 0 in subscribing to a long term deposit. Dichotomous meaning 2. Since both of our model’s dependent variables are binary, they meet this assumption.
Assumption 2 - “Have one or more independent variables that are continuous or categorical.”
Our independent variables in the case of Model 1 are both categorical and meet our assumptions. In Model 2 the first predictor is categorical which is housing and the second is a continuous data which is age. This means that Model 2 has met our assumptions.
Assumption 3 - “Linear relationship between continuous independent variables and the logit transformation of the dependent variables.”
In model 1 this does not meet our assumptions as our data is categorical and does not meet the assumption of linearity. In model 2 the 2 predictors are continuous and categorical data hence there should be a linear relationship between independent and dependent variables. Model 2, our results are statistically significant with P < 0.05. According to Tarling (2008), (VIF <2.5,Tolerance >0.4). Comparing it with the results that we have received. Our results are within acceptable levels, suggesting that there is a possibility of collinearity. VIF show’s that we only have little correlation. Model 2 meets this assumption.
Assumption 4 - “Independent variables should not be too highly correlated with each other”
For Model 1 our results are not statistically significant with P > 0.05. According to Tarling (2008), (VIF <2.5,Tolerance >0.4). Comparing it with the results that we have received. Our results are within acceptable levels, suggesting that there is a possibility of collinearity. VIF show’s that we only have little correlation. For Model 2, the answer is the same as assumption 3. Both models meet assumption 4.


Assumption 1 met?
Assumption 2 met?
Assumption 3 met?
Assumption 4 met?
MODEL 1
YES
YES
NO (both predictors are categorical)
YES
MODEL 2
YES
YES
NO
YES

General Assumption - Large sample size
A minimum of the cases is needed with the least frequent outcome for each variable that is independent in the model. We have 2 independent variables in both of our models and the expected probability and expected probability of the least frequent outcome when we look into the frequency of our variables. In our case the least frequent is 1 hence, the minimum sample size we need is 20 (10 * 2 / 1). We meet the general assumption for model 2. For model 1 our minimum sample size is 100 (10* 11 / 1).
MODEL 1 Independent variables
MODEL 3 Independent variables
count(bank_full$marital)
         x  freq
1 divorced  3553
2  married 17492
3   single  9443

count(bank_full$education)
                    x  freq
1            basic.4y  2380
2            basic.6y  1389
3            basic.9y  4276
4         high.school  7699
5          illiterate    11
6 professional.course  4321
7   university.degree 10412

lowest frequency is 11
> count(bank_full$housing)
    x  freq
1  no 13967
2 yes 16521

> count(bank_full$age)
    x freq
1  17    2
2  18   15
3  19   21
4  20   47
5  21   84
6  22  115
7  23  205
8  24  375
9  25  500
10 26  599
11 27  699
12 28  809
13 29 1263
14 30 1441
15 31 1643
16 32 1555
17 33 1524
18 34 1431
19 35 1399
20 36 1391
21 37 1140
22 38 1033
23 39 1035
24 40  807
25 41  906
26 42  793
27 43  741
28 44  677
29 45  664
30 46  659
31 47  600
32 48  670
33 49  516
34 50  552
35 51  460
36 52  506
37 53  469
38 54  432
39 55  400
40 56  407
41 57  372
42 58  383
43 59  266
44 60  156
45 61   64
46 62   42
47 63   41
48 64   45
49 65   36
50 66   40
51 67   21
52 68   30
53 69   30
54 70   41
55 71   46
56 72   28
57 73   28
58 74   29
59 75   20
60 76   28
61 77   12
62 78   20
63 79   12
64 80   26
65 81   16
66 82   13
67 83   15
68 84    4
69 85    7
70 86    3
71 87    1
72 88   22
73 89    2
74 91    2
75 94    1
76 95    1

lowest frequency 1
 
Section 4 – Discussion/Conclusion
In this section we reflect on our results from the perspective of our research question. Our main research question was, “Can a customer’s marital, education and age affect their choice to subscribe to a long-term deposit and is there a differential effect on customer’s with a loan such as housing?” We separated this research question into two hypotheses for model 1,‘Marital Status and education of a Customer will have an impact on the target variable’ and model 2,’Age and housing loan of a Customer will have an impact on the target variable’. In the results,  both of the model’s results we got was that p was < 0.05 which shows that they are highly significant. I believe the changes that I would have done in this research question was to not use housing as a predictor as an issue arises with the probability section. With shorter categories, possibly less than 6 unlike the housing variable. We could have a better and cleaner output. Also another possibility to improve the research question was adding more variables or changing age to duration as duration was a more interesting data set to work on. In the analysis we saw that the company was calling the customers multiple times and there were no more people subscribing to the term deposit after the 9th call attempt. I believe this will have a huge impact for the company to show to them that after X amounts of calls the probability of customers subscribing is X%. 



References
Moro, S., Cortes, P., & Rita, P. (2014). UCI Machine Learning Repository: Bank Marketing Data Set.Https://Archive.Ics.Uci.Https://Archive.Ics.Uci.Edu/Ml/Datasets/Bank+Marketingedu/Ml/Datasets/Bank+Marketing. https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
Soon, Y. K., Mann, V., Wei, L. L. T., & Long, L. S. (2018, December 16). RPubs - Portuguese Bank MarketingData.Https://Rpubs.Com/Shienlong/Wqd7004_RRookie. https://rpubs.com/shienlong/wqd7004_RRookie
Laerd Statistics. (n.d.). How to perform a Binomial Logistic Regression in SPSS Statistics | Laerd Statistics. Https://Statistics.Laerd.Com/Spss-Tutorials/Binomial-Logistic-Regression-Using-Spss-Statistics.Php. https://statistics.laerd.com/spss-tutorials/binomial-logistic-regression-using-spss-statistics.php
Lawless, D. (2020). Prob. and Statistical Inference MATH9102: 2020:21. Brightspace Notes. https://brightspace.tudublin.ie/d2l/le/content/126289/Home
Tarling, R. (2008). Statistical Modelling for Social Researchers: Principles and Practice. Routledge.

