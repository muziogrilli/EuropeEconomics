library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis

# Clean R Environment
rm(list = ls())

# Define local working directory
setwd("C:/Users/MG53060/Documents/GitHub/EuropeEconomics")

# Retrieve Randomized Sample Data for the years 2015-2016-2017-2018
Data <- readRDS("data_ds_15_18.rds")

# Rename country column
Data$country  <- Data$Country.ISO.Code

# Rename firm index column
Data$firm_index <- Data$BvD.ID.number 

# Rename sector column
Data$sector  <- Data$nace_div

# Add innovations levels
Data$innovationLevel <- Data$country
Data$innovationLevel <- as.factor(Data$innovationLevel)
# Change levels:
# Missing: GB, GR
levels(Data$innovationLevel) <- list(
  modest   = c("RO", "BG"),
  moderate = c("HR", "PL", "LV", "HU", "SK", "EL", "LT", "IT", "MT", "CZ", "SI", "ES", "CY", "GR"),
  strong   = c("PT", "EE", "FR", "IE", "AT", "DE", "BE", "GB"),
  leader   = c("LU", "NL", "DK", "FI", "SE")
)

# Add logarithm of revenues per employee
Data$log_rev_empl <- log(Data$rev_empl)

# Remove outliers by using the datavalues from the cleaned data for cross sectional data regression
cleanData <- readRDS(file = "./CleanData.rds")

Data_no <- Data[Data$BvD.ID.number%in%cleanData$BvD.ID.number,]

# remove infinte values rows
Data_c <- Data_no %>% 
          filter_if(~is.numeric(.), all_vars(is.finite(.)))

# Declare dataset as panel data
dataPanel <- pdata.frame(Data_c, index=c("firm_index","year"), drop.index=FALSE, row.names=TRUE)
# Subgrouping for SME 
dataPanel_sme <- filter(dataPanel, sme == 'sme')
# Subgrouping for Large
dataPanel_large <- filter(dataPanel, sme == 'large')
# Subgrouping for Modest and Moderate
dataPanel_MM <- filter(dataPanel, innovationLevel == 'modest' |  innovationLevel == 'moderate')
# Subgrouping for Modest and Moderate
dataPanel_SL <- filter(dataPanel, innovationLevel == 'strong' |  innovationLevel == 'leader')

# Exploratory Data Analysis
#p1 <- coplot(rev_empl ~ year|country, type="b", data=dataPanel) 
#print(p1)

#p2 <- scatterplot(log_rev_empl~year|country, data=dataPanel)
#print(p2)

# Eterogeneity across country
#p3 <- plotmeans(log_rev_empl ~ country, data = dataPanel)
#print(p3)

# Eterogeneity across sector
#p4 <- plotmeans(log_rev_empl ~ sector, data = dataPanel)
#print(p4)

# Eterogeneity across years
p5 <- plotmeans(log_rev_empl ~ year, data = dataPanel)
print(p5)


# Perform random effects regression - Model 1

# Full Sample
model1_re       <- plm(log_rev_empl ~ factor(ip_owner) + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel      , model="random")
# SME
model1_re_sme   <- plm(log_rev_empl ~ factor(ip_owner) + factor(country) + factor(sector) + factor(year)               + age + log(employment+1), data = dataPanel_sme  , model="random")
# Large Companies
model1_re_large <- plm(log_rev_empl ~ factor(ip_owner) + factor(country) + factor(sector) + factor(year)               + age + log(employment+1), data = dataPanel_large, model="random")
# Modest Moderate Innovator countries
model1_re_MM    <- plm(log_rev_empl ~ factor(ip_owner) + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel_MM   , model="random")
# Strong  Leader Innovator countries
model1_re_SL    <- plm(log_rev_empl ~ factor(ip_owner) + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel_SL   , model="random")

# Perform random effects regression - Model 2
model2_re       <- plm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel      , model="random")
# SME
model2_re_sme   <- plm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + factor(country) + factor(sector) + factor(year) +               age + log(employment+1), data = dataPanel_sme  , model="random")
# Large Companies 
model2_re_large <- plm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + factor(country) + factor(sector) + factor(year) +               age + log(employment+1), data = dataPanel_large, model="random")
# Modest Moderate Innovator countries
model2_re_MM    <- plm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel_MM   , model="random")
# Strong  Leader Innovator countries
model2_re_SL    <- plm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + factor(country) + factor(sector) + factor(year) + factor(sme) + age + log(employment+1), data = dataPanel_SL   , model="random")

