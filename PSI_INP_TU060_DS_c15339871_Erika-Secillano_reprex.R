#' ---
#' output:
#'   md_document:
#'     pandoc_args:
#'       - '--from=markdown-implicit_figures'
#'       - '--to=commonmark'
#'       - '--wrap=preserve'
#' ---



#+ reprex-setup, include = FALSE
options(tidyverse.quiet = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)

#+ reprex-body
###LOAD###
library(knitr)      # web widget
library(tidyverse)  # data manipulation
library(data.table) # fast file reading
library(caret)      # rocr analysis
library(ROCR)       # rocr analysis
library(kableExtra) # nice table html formating 
library(gridExtra)  # arranging ggplot in grid
library(rpart)      # decision tree
library(rpart.plot) # decision tree plotting
library(caTools)    # split 
library(colorspace)
library(grid)
library(VIM)    # split 
library(corrplot)
library(ggplot2)
library(plyr)
library(psych)
library(dplyr)
require(tidyr)
require(lazyeval)
library(Epi)#ROC Curve
library(nnet)
####DESCRIPTIVE STATISTICS####
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

#Replace the target variables yes = 1, no = 0
bank_full$y<-ifelse(bank_full$y =='yes', 1,0)
bank_full$y<-as.factor(bank_full$y)

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
corrplot.mixed(cor(new_df),
               lower = "number", 
               upper = "circle",
               tl.col = "black")

#rename y to Target
colnames(bank_full)[colnames(bank_full) == 'y'] <- 'target'

#####CATEGORICAL VARIABLES PLOT######
#plot the categorical variables change job to the relevant column to plot
ggplot(bank_full %>% count(job, target) %>%    
         mutate(pct=n/sum(n),               
                ypos = cumsum(n) - 0.5*n), 
       aes(job, n, fill=target)) +
  geom_bar(stat="identity")


ggplot(bank_full %>% count(day_of_week, target) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(day_of_week, n, fill=target)) +
  geom_bar(stat="identity")

ggplot(bank_full %>% count(housing, target) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(housing, n, fill=target)) +
  geom_bar(stat="identity")

ggplot(bank_full %>% count(marital, target) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(marital, n, fill=target)) +
  geom_bar(stat="identity")

ggplot(bank_full %>% count(poutcome, target) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(poutcome, n, fill=target)) +
  geom_bar(stat="identity")

ggplot(bank_full %>% count(default, target) %>%    # Group by region and species, then count number in each group
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(default, n, fill=target)) +
  geom_bar(stat="identity")
#####CATEGORICAL VARIABLES PLOT END######


#####PLOT OTHER VARIABLES AGE####
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
  facet_grid(cols = vars(target)) +
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
grid.arrange(age_dis,age_box_plot,age_marital,age_sub, ncol = 2, nrow = 2)
#####PLOT OTHER VARIABLES AGE END####

#####PLOT OTHER VARIABLES EDU + SUBS####

#Education X subscription
ggplot(data = bank_full, aes(x=education, fill=target)) +
  geom_bar() +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

#####PLOT OTHER VARIABLES EDU + END####

#####PLOT OTHER VARIABLES CAMPAIGN####
#Subscription based on the campaign
ggplot(data=bank_full, aes(x=campaign, fill=target))+
  geom_histogram()+
  ggtitle("Subscription based during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

#####PLOT OTHER VARIABLES CAMPAIGN####


#####PLOT OTHER VARIABLES DURATION####

##Duration
range(bank_full$duration)
#[1]    0 4918

summary(bank_full$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   102.0   180.0   258.3   319.0  4918.0 

bank_full %>% select(duration) %>% arrange(desc(duration)) %>% head

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

#####PLOT OTHER VARIABLES DURATION####

#############################STATISTICAL EVDENCE#################################


#Replace all unknown with NA
bank_full[bank_full=="unknown"] <- NA

#remove na
bank_full <- bank_full[complete.cases(bank_full),]

#plot and make numeric
multi.hist(bank_full[,sapply(bank_full, is.numeric)])

pastecs::stat.desc(bank_full$age, basic=F)

#QQplot age
qqnorm(bank_full$age); qqline(bank_full$age,col ="steelblue", lwd = 2)

#Homogeneity of Variance
bank_full %>% gather(age, key = 'var', value = 'value') %>%
  ggplot(aes(x = var, y = value, fill = housing)) +
  geom_boxplot() +
  theme_bw()

##LAVENE TEST
car::leveneTest(age ~ housing, data=bank_full)
car::leveneTest(age ~ housing, data=bank_full, center = 'mean')

##T-TEST
stats::t.test(age~housing,var.equal=TRUE,data=bank_full)

psych::describeBy(bank_full$age, bank_full$housing, mat=TRUE)

res <- stats::t.test(age~housing,var.equal=TRUE,data=bank_full)

effectsize::t_to_d(t = res$statistic, res$parameter)


effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes


#############################STATISTICAL EVDENCE END#################################

#############################MODEL 1#################################
#LOGISTIC MODEL
logmodel1 <- glm(target ~ marital + education, data = bank_full, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel1)

#Chi-square plus significance
lmtest::lrtest(logmodel1)

## odds ratios 
cbind(Estimate=round(coef(logmodel1),4),
      OR=round(exp(coef(logmodel1)),4))

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

#Chi-square plus significance
lmtest::lrtest(logmodel1)

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel1, which="CoxSnell")

DescTools::PseudoR2(logmodel1, which="Nagelkerke")

#Output the marital and education and ROC plot
ROC(form=target ~ marital+education, data=bank_full,plot="ROC")

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
generalhoslem::logitgof(bank_full$target,fitted(logmodel1))

#Collinearity
vifmodel<-car::vif(logmodel1)
vifmodel

#Tolerance
1/vifmodel

#############################MODEL 1 END#################################

#############################MODEL 2 END#################################
#LOGISTIC MODEL
logmodel2 <- glm(target ~ housing + age, data = bank_full, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel2)

#Chi-square plus significance
lmtest::lrtest(logmodel2)

## odds ratios 
cbind(Estimate=round(coef(logmodel2),4),
      OR=round(exp(coef(logmodel2)),4))

# Probability of answering yes when the loan is no 
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0)

#Probability of answering yes when the loan is no
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1)

#Probability of answering yes when the loan is no and age
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*0 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

#Probability of answering yes when the loan is yes and age
arm::invlogit(coef(logmodel2)[1]+ coef(logmodel2)[2]*1 +coef(logmodel2)[3]*0+coef(logmodel2)[3]*1)

#Chi-square plus significance
lmtest::lrtest(logmodel2)

#Pseudo Rsquared 
DescTools::PseudoR2(logmodel2, which="CoxSnell")

DescTools::PseudoR2(logmodel2, which="Nagelkerke")

#Output the marital and education and ROC plot
ROC(form=target ~ housing + age, data=bank_full,plot="ROC")

#Check the assumption of linearity of independent variables and log odds using a Hosmer-Lemeshow test, if this is not statistically significant we are ok
generalhoslem::logitgof(bank_full$target,fitted(logmodel2))

#Collinearity
vifmodel<-car::vif(logmodel2)
vifmodel

#Tolerance
1/vifmodel
#############################MODEL 2 END#################################


#general assumption to count and the frequency
count(bank_full$marital)

count(bank_full$education)

count(bank_full$housing)

count(bank_full$age)






library(reprex)
r.file <- "PSI_INP_TU060_DS_c15339871_Erika-Secillano.r" ## path to your R script file
reprex(input = r.file, outfile = NA)



#' <sup>Created on `r Sys.Date()` by the [reprex package](https://reprex.tidyverse.org) (v`r utils::packageVersion("reprex")`)</sup>


