library(ggplot2)
library(plyr)
library(dplyr)
library(jtools)
library(stargazer)

# Define local working directory
setwd("C:/Users/muzio/Documents/Economist Unit/EuropeEconomics")

# Retrieve Randomized Sample Data provided used as basis for Table 6
Data <- readRDS("data_ds_2018.rds")

# print column names on terminal
colnames(Data)

# Internal function to remove columns with NA for the following columns
# employment:       number of employees 
# rev_empl:   revenues per employee
# turnover:   revenues
# age:        age of company

# Add column with logarithm of revenues per employee
Data$log_rev_empl <- log(Data$rev_empl)
Data$ip_owner <- !(Data$ip_non_owner)

completeVec <- complete.cases(Data[, c("turnover","rev_empl", "employment", "age", "log_rev_empl")])
cleanData <- Data[completeVec, ]

# plotting
plotFeature <- "log_rev_empl"
plotIndicator <- "ip_owner"

# Outliers removal procedure using interquartile range
# The interquartile range (IQR) is the difference between 
# the 75th percentile (Q3) and the 25th percentile (Q1) in a dataset. 
# It measures the spread of the middle 50% of values. 
# An observation is an outlier if it is 1.5 times the interquartile range greater 
# than the third quartile (Q3) or 1.5 times the interquartile range less 
# than the first quartile (Q1).

# compute first quantile
Q1 <- quantile(cleanData[,plotFeature], .25)
# compute third quantile
Q3 <- quantile(cleanData[,plotFeature], .75)
# compute inter-quantile range
IQR <- IQR(cleanData[,plotFeature])

# only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
cleanData_no <- subset(cleanData, cleanData[,plotFeature] > (Q1 - 1.5*IQR) & cleanData[,plotFeature] < (Q3 + 1.5*IQR))

# view row and column count of new data frame
dim(cleanData_no) 

# define plot list
plot_list = list()

# perform histogram plot of revenue per employee data
plotData <- cleanData_no[,c("ip_owner",plotFeature)]

# 1. Histogram using the (rev_empl) distinguishing by ip ownership

mu_1 <- ddply( cleanData_no[,c("ip_owner","rev_empl")], plotIndicator, summarise, grp.mean=mean(rev_empl) )

p<-ggplot(cleanData_no[,c("ip_owner","rev_empl")], aes_string(x="rev_empl", color=plotIndicator)) +
  geom_histogram(fill="white", position="identity")+
  geom_vline(data=mu_1, aes(xintercept=grp.mean, color=ip_owner),
             linetype="dashed")+
  theme(legend.position="top")

# save plot to list
plot_list[[1]] <- p
print(plot_list[[1]])

# 2. Histogram using the log(rev_empl) distinguishing by ip ownership

mu_2 <- ddply( plotData, plotIndicator, summarise, grp.mean=mean(log_rev_empl) )

p<-ggplot(plotData, aes_string(x=plotFeature, color=plotIndicator)) +
  geom_histogram(fill="white", position="identity")+
  geom_vline(data=mu_2, aes(xintercept=grp.mean, color=ip_owner),
             linetype="dashed")+
  theme(legend.position="top")

# save plot to list
plot_list[[2]] <- p
print(plot_list[[2]])

# 3. Histogram using the log(rev_empl) distinguishing by SME
cleanData_no$sme <- as.factor(cleanData_no$sme)
mu_3 <- ddply( cleanData_no[,c("sme","log_rev_empl")], "sme", summarise, grp.mean=mean(log_rev_empl) )

p<-ggplot(cleanData_no[,c("sme","log_rev_empl")], aes_string(x="log_rev_empl", color="sme")) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu_3, aes(xintercept=grp.mean, color=sme),
             linetype="dashed")+
  theme(legend.position="top")

# save plot to list
plot_list[[3]] <- p
print(plot_list[[3]])

# Econometrics Models

# Define dataset regression data
regData <- cleanData_no[, c("employment","ip_owner","patent_only", "tm_only", "des_only", "pat_tm","pat_des","tm_des","pat_tm_des","sme","age","n_ep_pats_stock","n_nat_pats_stock","n_eu_tm_stock","n_nat_tm_stock","n_eu_des_stock","n_nat_des_stock","log_rev_empl")]

# Add variables needed for stock analysis

# Patents
regData$neppats <- log((regData$n_ep_pats_stock / regData$employment ) + 1.0) 
regData$nnatpats <- log((regData$n_nat_pats_stock / regData$employment) + 1.0)
# Trademarks
regData$neutm <- log((regData$n_eu_tm_stock / regData$employment) + 1.0) 
regData$nnattm <- log((regData$n_nat_tm_stock / regData$employment) + 1.0) 
# Design
regData$neudes <- log((regData$n_eu_des_stock / regData$employment) + 1.0) 
regData$nnatdes <- log((regData$n_nat_des_stock / regData$employment) + 1.0)

# MODEL 1

# Perform regression of log_rev_empl using a log-linear model and indicator variable "ip_owner"

# Perform regression
reg1 = lm(log_rev_empl ~ ip_owner, data = regData)
print("----------------------")
print("-- Model 1 - Global --")
print("----------------------")
print(summ(reg1))

# MODEL 2

# Perform regression of log_rev_empl using a log-linear model and indicator variables 
# "ip_owner"
# "sme"

# Perform regression
reg2 = lm(log_rev_empl ~ ip_owner + sme, data = regData)
print("----------------------")
print("-- Model 2 - Global --")
print("----------------------")
print(summ(reg2))

# MODEL 3

# Perform regression of log_rev_empl using a log-linear model and  variables 
# "ip_owner"
# "sme"
# "age"

# Perform regression
reg3 = lm(log_rev_empl ~ ip_owner + sme + age, data = regData)
print("----------------------")
print("-- Model 3 - Global --")
print("----------------------")
print(summ(reg3))

# Generate output tables
stargazer(reg3, type = "text", title="Table 15 part 1", align=TRUE, out="table15_1.tex")

# MODEL 4

# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# patent_only - company owns only patents
# tm_only     - company owns only trademarks
# des_only    - company owns only design protections
# pat_tm      - company owns      patents and trademarks
# pat_des     - company owns      patents and designs
# tm_des      - company owns      trademarks and designs
# pat_tm_des  - company owns      patents, trademarks and designs


# Perform regression
reg4 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des, data = regData)
print("----------------------")
print("-- Model 4 - Global --")
print("----------------------")
print(summ(reg4))

# MODEL 5

# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# patent_only - company owns only patents
# tm_only     - company owns only trademarks
# des_only    - company owns only design protections
# pat_tm      - company owns      patents and trademarks
# pat_des     - company owns      patents and designs
# tm_des      - company owns      trademarks and designs
# pat_tm_des  - company owns      patents, trademarks and designs
# sme         - is the company an SME or a LARGE company

# Perform regression
reg5 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme, data = regData)
print("----------------------")
print("-- Model 5 - Global --")
print("----------------------")
print(summ(reg5))

# MODEL 6

# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# patent_only - company owns only patents
# tm_only     - company owns only trademarks
# des_only    - company owns only design protections
# pat_tm      - company owns      patents and trademarks
# pat_des     - company owns      patents and designs
# tm_des      - company owns      trademarks and designs
# pat_tm_des  - company owns      patents, trademarks and designs
# sme         - is the company an SME or a LARGE company
# age         - age of company


# Perform regression
reg6 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age, data = regData)
print("----------------------")
print("-- Model 6 - Global --")
print("----------------------")
print(summ(reg6))

# Generate output tables
stargazer(reg6, type = "text", title="Table 15 part 2", align=TRUE, out="table15_2.tex")

# Repeat Analysis on the Subset belonging to SME Group

regData_sme <- filter(regData, sme == 'sme')

# Model 1
reg1_sme = lm(log_rev_empl ~ ip_owner, data = regData_sme)
print("----------------------")
print("-- Model 1 - SME -----")
print("----------------------")
print(summ(reg1_sme))

# Model 2
reg2_sme = lm(log_rev_empl ~ ip_owner + age, data = regData_sme)
print("----------------------")
print("-- Model 2 - SME -----")
print("----------------------")
print(summ(reg2_sme))

# Generate output tables
stargazer(reg2_sme, type = "text", title="Table 16 SME Model 1)", align=TRUE, out="table16_1.tex")

# Model 3
reg3_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des, data = regData_sme)
print("----------------------")
print("-- Model 3 - SME -----")
print("----------------------")
print(summ(reg3_sme))

# Model 4
reg4_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age, data = regData_sme)
print("----------------------")
print("-- Model 4 - SME -----")
print("----------------------")
print(summ(reg4_sme))

# Generate output tables
stargazer(reg4_sme, type = "text", title="Table 16 SME Model 2)", align=TRUE, out="table16_2.tex")

# Repeat Analysis on the Subset belonging to Large Group

regData_large <- filter(regData, sme == 'large')

# Model 1
reg1_large = lm(log_rev_empl ~ ip_owner, data = regData_large)
print("----------------------")
print("-- Model 1 - LARGE ---")
print("----------------------")
print(summ(reg1_large))

# Model 2
reg2_large = lm(log_rev_empl ~ ip_owner + age, data = regData_large)
print("----------------------")
print("-- Model 2 - LARGE ---")
print("----------------------")
print(summ(reg2_large))

# Generate output tables
stargazer(reg2_large, type = "text", title="Table 16 Large Model 1)", align=TRUE, out="table16_3.tex")

# Model 3
reg3_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des, data = regData_large)
print("----------------------")
print("-- Model 3 - LARGE ---")
print("----------------------")
print(summ(reg3_large))

# Model 4
reg4_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age, data = regData_large)
print("----------------------")
print("-- Model 4 - LARGE ---")
print("----------------------")
print(summ(reg4_large))

# Generate output tables
stargazer(reg4_large, type = "text", title="Table 16 Large Model 2)", align=TRUE, out="table16_4.tex")

# Analysis based on stocks

# Model 1
reg_s = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes, data = regData)
print("----------------------")
print("-- Model 1 - STOCK ---")
print("----------------------")
print(summ(reg_s))

# Generate output tables
stargazer(reg_s, type = "text", title="Table 17)", align=TRUE, out="table17.tex")

# Model 1  SME
reg_s_sme = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes, data = regData_sme)
print("---------------------------")
print("-- Model 1 - STOCK - SME --")
print("---------------------------")
print(summ(reg_s_sme))

# Generate output tables
stargazer(reg_s_sme, type = "text", title="Table 18 part 1)", align=TRUE, out="table18_1.tex")

# Model 2 Large
reg_s_large = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes, data = regData_large)
print("-----------------------------")
print("-- Model 1 - STOCK - Large --")
print("-----------------------------")
print(summ(reg_s_large))

# Generate output tables
stargazer(reg_s_large, type = "text", title="Table 18 part 2)", align=TRUE, out="table18_2.tex")


