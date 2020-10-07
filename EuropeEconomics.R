library(ggplot2)
library(plyr)
library(dplyr)
library(jtools)
library(stargazer)
library(car)

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

# country variable = Country.ISO.Code
# sector variable = nace_div

cleanData_no$country <- cleanData_no$Country.ISO.Code
cleanData_no$sector  <- cleanData_no$nace_div

# Define vector of variables
variables <- c("country"         , "sector",
               "employment"      , "ip_owner"     , "patent_only"   , "tm_only", 
               "des_only"        , "pat_tm"       , "pat_des"       , "tm_des"  ,
               "pat_tm_des"      , "sme"          , "age"           , "n_ep_pats_stock", 
               "n_nat_pats_stock", "n_eu_tm_stock", "n_nat_tm_stock", "n_eu_des_stock"  ,
               "n_nat_des_stock" , "log_rev_empl")

# Define dataset regression data
regData <- cleanData_no[, variables]
regData$country <- as.factor(regData$country)
regData$sector <- as.factor(regData$sector)

# Add innovation levels
regData$innovationLevel <- regData$country
# Change levels:
# Missing: GB, GR
levels(regData$innovationLevel) <- list(
  modest   = c("RO", "BG"),
  moderate = c("HR", "PL", "LV", "HU", "SK", "EL", "LT", "IT", "MT", "CZ", "SI", "ES", "CY", "GR"),
  strong   = c("PT", "EE", "FR", "IE", "AT", "DE", "BE", "GB"),
  leader   = c("LU", "NL", "DK", "FI", "SE")
)
# 29 countries
regData$innovationLevel <- as.factor(regData$innovationLevel)

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

# MODEL 1: Table 15 portion 1
# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# ip owner    - company owns ip
# sme         - is the company an SME or a LARGE company
# age         - age of company
# country     - company´s country
# sector      - sector

# Perform regressions

# Submodel 1: this model represents a pooled model where the data on different firms
#             are simply pulled together with no provisions for individual differences
#             Individual heterogeneity is therefore not allowed.
reg1 = lm(log_rev_empl ~ ip_owner + sme + age + ip_owner * sme                   , data = regData)
reg2 = lm(log_rev_empl ~ ip_owner + sme + age + ip_owner * sme + country         , data = regData)
reg3 = lm(log_rev_empl ~ ip_owner + sme + age + ip_owner * sme           + sector, data = regData)
reg4 = lm(log_rev_empl ~ ip_owner + sme + age + ip_owner * sme + country + sector, data = regData)
reg5 = lm(log_rev_empl ~ ip_owner + sme + age +                  country + sector, data = regData)

# Generate output tables
stargazer(reg5, reg4, reg3, reg2, reg1, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("IP Owner", "SME", "Age", "IP Owner * SME"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 part 1", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_1.tex")

# MODEL 2: Table 15 portion 2

# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# sme         - is the company an SME or a LARGE company
# age         - age of company
# patent_only - company owns only patents
# tm_only     - company owns only trademarks
# des_only    - company owns only design protections
# pat_tm      - company owns      patents and trademarks
# pat_des     - company owns      patents and designs
# tm_des      - company owns      trademarks and designs
# pat_tm_des  - company owns      patents, trademarks and designs

# Perform regression

reg5 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age                   , data = regData)
reg6 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country         , data = regData)
reg7 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age           + sector, data = regData)
reg8 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country + sector, data = regData)

# Generate output tables
stargazer(reg8, reg7, reg6, reg5, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "SME", "Age"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 part 2", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2.tex")

# Repeat Analysis on the Subset belonging to SME Group
regData_sme <- filter(regData, sme == 'sme')

# Model 1 - Table 16 SME Model 1
reg1_sme = lm(log_rev_empl ~ ip_owner + age                   , data = regData_sme)
reg2_sme = lm(log_rev_empl ~ ip_owner + age + country         , data = regData_sme)
reg3_sme = lm(log_rev_empl ~ ip_owner + age           + sector, data = regData_sme)
reg4_sme = lm(log_rev_empl ~ ip_owner + age + country + sector, data = regData_sme)

# Generate output tables
stargazer(reg4_sme, reg3_sme, reg2_sme, reg1_sme, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("IP Owner", "Age"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 1 - SME", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table16_1_sme.tex")

# Model 2 - Table 16 SME Model 2
reg5_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                   , data = regData_sme)
reg6_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country         , data = regData_sme)
reg7_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector, data = regData_sme)
reg8_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector, data = regData_sme)

# Generate output tables
stargazer(reg8_sme, reg7_sme, reg6_sme, reg5_sme, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 2 - SME", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table16_2_sme.tex")

# Repeat Analysis on the Subset belonging to Large Group

regData_large <- filter(regData, sme == 'large')

# Model 1 - Table 16 SME Model 1
reg1_large = lm(log_rev_empl ~ ip_owner + age                   , data = regData_large)
reg2_large = lm(log_rev_empl ~ ip_owner + age + country         , data = regData_large)
reg3_large = lm(log_rev_empl ~ ip_owner + age           + sector, data = regData_large)
reg4_large = lm(log_rev_empl ~ ip_owner + age + country + sector, data = regData_large)

# Generate output tables
stargazer(reg4_large, reg3_large, reg2_large, reg1_large, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("IP Owner", "Age"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 1 - Large", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table16_1_large.tex")

# Model 2 - Table 16 SME Model 2
reg5_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                   , data = regData_large)
reg6_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country         , data = regData_large)
reg7_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector, data = regData_large)
reg8_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector, data = regData_large)

# Generate output tables
stargazer(reg8_large, reg7_large, reg6_large, reg5_large, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age"), omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 2 - Large", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table16_2_large.tex")

###############################
# Stocks Analysis
###############################

# Model 1 - Table 17
reg_s1 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + sme                   , data = regData)
reg_s2 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + sme + country         , data = regData)
reg_s3 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + sme +           sector, data = regData)
reg_s4 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + sme + country + sector, data = regData)

# Generate output tables
reg_s_cov_labels <- c("$log(N_{EU \\, Patents}/N_{Employee})$", "$log(N_{NAT \\,  Patents}/N_{Employee})$", "$log(N_{EU \\,  TM}/N_{Employee})$", "$log(N_{NAT \\,  TM}/N_{Employee})$", "$log(N_{EU \\, Design}/N_{Employee})$", "$log(N_{NAT \\,  Design}/N_{Employee})$", "SME")
stargazer(reg_s4, reg_s3, reg_s2,reg_s1, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_s_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 17", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table17.tex")

# Model 1  SME
reg_s_sme1 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes                   , data = regData_sme)
reg_s_sme2 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + country         , data = regData_sme)
reg_s_sme3 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes +           sector, data = regData_sme)
reg_s_sme4 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + country + sector, data = regData_sme)

# Generate output tables
reg_s_sme_cov_labels <- c("$log(N_{EU \\, Patents}/N_{Employee})$", "$log(N_{NAT \\,  Patents}/N_{Employee})$", "$log(N_{EU \\,  TM}/N_{Employee})$", "$log(N_{NAT \\,  TM}/N_{Employee})$", "$log(N_{EU \\, Design}/N_{Employee})$", "$log(N_{NAT \\,  Design}/N_{Employee})$")
stargazer(reg_s_sme4, reg_s_sme3, reg_s_sme2, reg_s_sme1, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_s_sme_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 18 SME", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table18_sme.tex")

# Model 1 Large
reg_s_large1 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes                   , data = regData_large)
reg_s_large2 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + country         , data = regData_large)
reg_s_large3 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes +           sector, data = regData_large)
reg_s_large4 = lm(log_rev_empl ~ neppats + nnatpats + neutm + nnattm + neudes + nnatdes + country + sector, data = regData_large)

# Generate output tables
reg_s_large_cov_labels <- c("$log(N_{EU \\, Patents}/N_{Employee})$", "$log(N_{NAT \\,  Patents}/N_{Employee})$", "$log(N_{EU \\,  TM}/N_{Employee})$", "$log(N_{NAT \\,  TM}/N_{Employee})$", "$log(N_{EU \\, Design}/N_{Employee})$", "$log(N_{NAT \\,  Design}/N_{Employee})$")
stargazer(reg_s_large4, reg_s_large3, reg_s_large2, reg_s_large1, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_s_large_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 18 Large", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table18_large.tex")

###################################
# Innovation Scoreboard Analysis
###################################

# Define common covariates labels
cov_labes_is <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "SME", "Age")

regData_modest <- filter(regData, innovationLevel == 'modest')

reg5_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age                   , data = regData_modest)
reg6_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country         , data = regData_modest)
reg7_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age           + sector, data = regData_modest)
reg8_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country + sector, data = regData_modest)

# Generate output tables
stargazer(reg8_modest, reg7_modest, reg6_modest, reg5_modest, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=cov_labes_is, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Modest", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2_modest.tex")

# Repeat Analysis on the Subset belonging to Moderate Innovators Group
regData_moderate <- filter(regData, innovationLevel == 'moderate')

reg5_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age                   , data = regData_moderate)
reg6_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country         , data = regData_moderate)
reg7_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age           + sector, data = regData_moderate)
reg8_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country + sector, data = regData_moderate)

# Generate output tables
stargazer(reg8_moderate, reg7_moderate, reg6_moderate, reg5_moderate, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=cov_labes_is, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Moderate", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2_moderate.tex")

# Repeat Analysis on the Subset belonging to Strong Innovators Group
regData_strong <- filter(regData, innovationLevel == 'strong')

reg5_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age                   , data = regData_strong)
reg6_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country         , data = regData_strong)
reg7_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age           + sector, data = regData_strong)
reg8_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country + sector, data = regData_strong)

# Generate output tables
stargazer(reg8_strong, reg7_strong, reg6_strong, reg5_strong, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=cov_labes_is, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Strong", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2_strong.tex")

# Repeat Analysis on the Subset belonging to Leader Innovators Group
regData_leader <- filter(regData, innovationLevel == 'leader')

reg5_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age                   , data = regData_leader)
reg6_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country         , data = regData_leader)
reg7_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age           + sector, data = regData_leader)
reg8_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + sme + age + country + sector, data = regData_leader)

# Generate output tables
stargazer(reg8_leader, reg7_leader, reg6_leader, reg5_leader, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=cov_labes_is, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Strong", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2_leader.tex")

# Generate output tables
stargazer(reg8_modest, reg8_moderate, reg8_strong, reg8_leader, type = "latex", style="jpam", column.labels=c("Modest","Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=cov_labes_is, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out="table15_2_classcomparison.tex")