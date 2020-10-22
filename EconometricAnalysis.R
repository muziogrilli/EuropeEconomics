library(dplyr)
library(stargazer)

# Clean R Environment
rm(list = ls())

# Define local working directory
setwd("C:/Users/MG53060/Documents/GitHub/EuropeEconomics")

# Retrieve Randomized Sample Data for the years 2015-2016-2017-2018
Data <- readRDS("data_ds_15_18.rds")

# Rename country column
Data$country  <- Data$Country.ISO.Code
# Rename sector column
Data$sector  <- Data$nace_div


# Define columns along which the averaging procedure should be performed
average_cols <- c("age"              , "employment"      , "turnover", 
                  "n_ep_pats_stock"  , "n_nat_pats_stock",
                  "n_eu_tm_stock"    , "n_nat_tm_stock"  , 
                  "n_eu_des_stock"   , "n_nat_des_stock" )

# Perform averaging procedure
# na.rm is set to TRUE: if there are missing values for the one or more years
#                       such values are discarded and the averaging is performed on
#                       the rest. NA is returned only if the entry is missing for all the years
# !!Employment: this variable is an integer which will be then turned into double

average_data <- Data %>% 
                group_by(BvD.ID.number) %>%  
                summarise_at(average_cols, mean, na.rm=TRUE)

# Define columns along which the indicator variables should be retrieved
indicator_cols <- c("ip_owner"     , "sme"     , "country" , "sector"  ,
                    "patent_only"  , "tm_only" , "des_only", 
                    "pat_tm"       , "pat_des" , "tm_des"  ,
                    "pat_tm_des", "patent_owner", "tm_owner", "des_owner")

indicator_data <- Data %>% 
                  group_by(BvD.ID.number) %>%  
                  summarise_at(indicator_cols, max, na.rm=TRUE)

# Join tables
joined_data <- left_join(indicator_data, average_data, by="BvD.ID.number")
# joined columms
joined_cols <- c(indicator_cols,average_cols)

# Remove cases which are not complete
completeVec <- complete.cases(joined_data[, joined_cols])
cleanData <- joined_data[completeVec, ]

# Add revenues per employee variable 
cleanData$rev_empl <- cleanData$turnover / cleanData$employment 
# Add logarithm of revenues per employee
cleanData$log_rev_empl <- log(cleanData$rev_empl)
# remove infinte values rows
cleanData <- cleanData %>% 
             filter_if(~is.numeric(.), all_vars(is.finite(.)))


# Turn char columns into factors
cleanData$sme     <- as.factor(cleanData$sme)
cleanData$country <- as.factor(cleanData$country)
cleanData$sector  <- as.factor(cleanData$sector)

# Add innovations levels
cleanData$innovationLevel <- cleanData$country
# Change levels:
# Missing: GB, GR
levels(cleanData$innovationLevel) <- list(
  modest   = c("RO", "BG"),
  moderate = c("HR", "PL", "LV", "HU", "SK", "EL", "LT", "IT", "MT", "CZ", "SI", "ES", "CY", "GR"),
  strong   = c("PT", "EE", "FR", "IE", "AT", "DE", "BE", "GB"),
  leader   = c("LU", "NL", "DK", "FI", "SE")
)

# Add variables needed for stock analysis

# Patents
cleanData$neppats  <- log((cleanData$n_ep_pats_stock  + 1) / (cleanData$employment) ) 
cleanData$nnatpats <- log((cleanData$n_nat_pats_stock + 1) / (cleanData$employment) ) 
# Trademarks
cleanData$neutm    <- log((cleanData$n_eu_tm_stock    + 1) / (cleanData$employment) ) 
cleanData$nnattm   <- log((cleanData$n_nat_tm_stock   + 1) / (cleanData$employment) ) 
# Design
cleanData$neudes   <- log((cleanData$n_eu_des_stock   + 1) / (cleanData$employment) ) 
cleanData$nnatdes  <- log((cleanData$n_nat_des_stock  + 1) / (cleanData$employment) ) 

# Perform Outlier Removal procedure
baseFeature <- "log_rev_empl"

# compute first quantile
Q1 <- quantile(cleanData$log_rev_empl, .25)
# compute third quantile
Q3 <- quantile(cleanData$log_rev_empl, .75)
# compute inter-quantile range
IQR <- IQR(cleanData$log_rev_empl)

# Lower portion
el1 <- cleanData[cleanData[,baseFeature] < Q1 - 1.5*IQR,]
# Upper portion
el2 <- cleanData[cleanData[,baseFeature] > Q3 + 1.5*IQR,]

# join outliers tables
el<-rbind(el1,el2)

# Prepare outliers tables
el$country  <- as.character(el$country)
el$sme      <- as.character(el$sme)
el1$country <- as.character(el1$country)
el1$sme     <- as.character(el1$sme)
el2$country <- as.character(el2$country)
el2$sme     <- as.character(el2$sme)

prop_table_el1 <-  el1                                 %>% 
                   filter(sme=="large")                %>% 
                   group_by(country)                   %>% 
                   summarise(count = n() )             %>% 
                   mutate( prop = count / sum(count) ) %>% 
                   arrange(desc(prop))

prop_table_el2 <-  el2                                 %>% 
                   filter(sme=="large")                %>% 
                   group_by(country)                   %>% 
                   summarise(count = n() )             %>% 
                   mutate( prop = count / sum(count) ) %>% 
                   arrange(desc(prop))

prop_table_el <- el                                  %>% 
                 filter(sme=="large")                %>% 
                 group_by(country)                   %>% 
                 summarise(count = n() )             %>% 
                 mutate( prop = count / sum(count) ) %>% 
                 arrange(desc(prop))


# only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
cleanData_no <- subset(cleanData, cleanData[,baseFeature] > (Q1 - 1.5*IQR) & cleanData[,baseFeature] < (Q3 + 1.5*IQR))

# Save clean data for further processing
saveRDS(cleanData_no, file = "CleanData.rds")

# MODEL 1: Table 15 portion 1
# Perform regression of log_rev_empl using a log-linear model 

# ip owner        - company owns ip
# age             - age of company
# country         - company?s country
# sector          - sector
# sme             - is the company an SME or a LARGE company
# log(employment) - Number of company's employees

# Perform regressions

# Submodel 1: this model represents a pooled model where the data on different firms
#             are simply pulled together with no provisions for individual differences
#             Individual heterogeneity is therefore not allowed.
reg1 = lm(log_rev_empl ~ ip_owner + age                                              , data = cleanData_no)
reg2 = lm(log_rev_empl ~ ip_owner + age + country                                    , data = cleanData_no)
reg3 = lm(log_rev_empl ~ ip_owner + age +         + sector                           , data = cleanData_no)
reg4 = lm(log_rev_empl ~ ip_owner + age + country + sector                           , data = cleanData_no)

reg5 = lm(log_rev_empl ~ ip_owner + age + country + sector                           , data = cleanData_no)
reg6 = lm(log_rev_empl ~ ip_owner + age + country + sector + sme                     , data = cleanData_no)
reg7 = lm(log_rev_empl ~ ip_owner + age + country + sector +     + log(employment+1) , data = cleanData_no)
reg8 = lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = cleanData_no)

# Generate output tables
reg_cov_labels <- c("IP Owner", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_1.tex"
stargazer(reg8, reg7, reg6, reg5, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 1", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# MODEL 2: Table 15 portion 2

# Perform regression of log_rev_empl using a log-linear model and the indicator variables

# age             - age of company
# country         - company?s country
# sector          - sector
# sme             - is the company an SME or a LARGE company
# log(employment) - Number of company's employees
# patent_only     - company owns only patents
# tm_only         - company owns only trademarks
# des_only        - company owns only design protections
# pat_tm          - company owns      patents and trademarks
# pat_des         - company owns      patents and designs
# tm_des          - company owns      trademarks and designs
# pat_tm_des      - company owns      patents, trademarks and designs

# Perform regression

reg1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                   , data = cleanData_no)
reg2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country         , data = cleanData_no)
reg3 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector, data = cleanData_no)
reg4 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector, data = cleanData_no)

reg5 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = cleanData_no)
reg6 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = cleanData_no)
reg7 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = cleanData_no)
reg8 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = cleanData_no)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2.tex"
stargazer(reg8, reg7, reg6, reg5, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

######################################################
# Repeat Analysis on the Subset belonging to SME Group

regData_sme <- filter(cleanData_no, sme == 'sme')

# Model 1 - Table 16 SME Model 1
reg1_sme = lm(log_rev_empl ~ ip_owner + age                                       , data = regData_sme)
reg2_sme = lm(log_rev_empl ~ ip_owner + age + country                             , data = regData_sme)
reg3_sme = lm(log_rev_empl ~ ip_owner + age           + sector                    , data = regData_sme)
reg4_sme = lm(log_rev_empl ~ ip_owner + age + country + sector                    , data = regData_sme)
reg5_sme = lm(log_rev_empl ~ ip_owner + age + country + sector + log(employment+1), data = regData_sme)

# Generate output tables
reg_cov_labels <- c("IP Owner", "Age", "$Log(Employment)$")
out_file <- "MeanTable16_1_sme.tex"
stargazer(reg5_sme, reg4_sme, reg3_sme, reg2_sme, reg1_sme, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 1 - SME", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Model 2 - Table 16 SME Model 2
reg1_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                                        , data = regData_sme)
reg2_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country                              , data = regData_sme)
reg3_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                     , data = regData_sme)
reg4_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                     , data = regData_sme)
reg5_sme = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + log(employment+1) , data = regData_sme)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "$Log(Employment)$")
out_file <- "MeanTable16_2_sme.tex"
stargazer(reg5_sme, reg4_sme, reg3_sme, reg2_sme, reg1_sme, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 2 - SME", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

######################################################
# Repeat Analysis on the Subset belonging to Large Group

regData_large <- filter(cleanData_no, sme == 'large')

# Model 1 - Table 16 SME Model 1
reg1_large = lm(log_rev_empl ~ ip_owner + age                                       , data = regData_large)
reg2_large = lm(log_rev_empl ~ ip_owner + age + country                             , data = regData_large)
reg3_large = lm(log_rev_empl ~ ip_owner + age           + sector                    , data = regData_large)
reg4_large = lm(log_rev_empl ~ ip_owner + age + country + sector                    , data = regData_large)
reg5_large = lm(log_rev_empl ~ ip_owner + age + country + sector + log(employment+1), data = regData_large)

# Generate output tables
reg_cov_labels <- c("IP Owner", "Age", "$Log(Employment)$")
out_file <- "MeanTable16_1_large.tex"
stargazer(reg5_large, reg4_large, reg3_large, reg2_large, reg1_large, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 1 - Large", align=FALSE, font.size = "tiny", no.space = FALSE, float = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Model 2 - Table 16 SME Model 2
reg1_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                                        , data = regData_large)
reg2_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country                              , data = regData_large)
reg3_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                     , data = regData_large)
reg4_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                     , data = regData_large)
reg5_large = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + log(employment+1) , data = regData_large)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "$Log(Employment)$")
out_file <- "MeanTable16_2_large.tex"
stargazer(reg5_large, reg4_large, reg3_large, reg2_large, reg1_large, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 16 Model 2 - Large", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

###################################
# Innovation Scoreboard Analysis

# Modest Innovators Group
regData_modest <- filter(cleanData_no, innovationLevel == 'modest')

reg3_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_modest)
reg4_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_modest)
reg5_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_modest)
reg6_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_modest)
reg7_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_modest)
reg8_modest = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_modest)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2_modest.tex"
stargazer(reg8_modest, reg7_modest, reg6_modest, reg5_modest, reg4_modest, reg3_modest, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Modest", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Moderate Innovators Group
regData_moderate <- filter(cleanData_no, innovationLevel == 'moderate')

reg3_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_moderate)
reg4_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_moderate)
reg5_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_moderate)
reg6_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_moderate)
reg7_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_moderate)
reg8_moderate = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_moderate)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2_moderate.tex"
stargazer(reg8_moderate, reg7_moderate, reg6_moderate, reg5_moderate, reg4_moderate, reg3_moderate, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Moderate", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Strong Innovators Group
regData_strong <- filter(cleanData_no, innovationLevel == 'strong')

reg3_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_strong)
reg4_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_strong)
reg5_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_strong)
reg6_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_strong)
reg7_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_strong)
reg8_strong = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_strong)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2_strong.tex"
stargazer(reg8_strong, reg7_strong, reg6_strong, reg5_strong, reg4_strong, reg3_strong, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Moderate", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Leading Innovators Group
regData_leader <- filter(cleanData_no, innovationLevel == 'leader')

reg3_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_leader)
reg4_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_leader)
reg5_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_leader)
reg6_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_leader)
reg7_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_leader)
reg8_leader = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_leader)

# Generate output tables
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2_leader.tex"
stargazer(reg8_leader, reg7_leader, reg6_leader, reg5_leader, reg4_leader, reg3_leader, type = "latex", style="jpam", dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Moderate", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Subgroup 1 Innovators Group
regData_sub1 <- filter(cleanData_no, innovationLevel == 'modest' |  innovationLevel == 'moderate')

reg3_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_sub1)
reg4_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_sub1)
reg5_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_sub1)
reg6_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_sub1)
reg7_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_sub1)
reg8_sub1 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_sub1)

# Subgroup 2 Innovators Group
regData_sub2 <- filter(cleanData_no, innovationLevel == 'strong' |  innovationLevel == 'leader')

reg3_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector + sme                     , data = regData_sub2)
reg4_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector                           , data = regData_sub2)
reg5_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector                           , data = regData_sub2)
reg6_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme                     , data = regData_sub2)
reg7_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = regData_sub2)
reg8_sub2 = lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = regData_sub2)

# Generate comparison tables

# SME Based
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME")
out_file <- "MeanTable15_2_classcomparison_1_1.tex"
stargazer(reg6_modest, reg6_moderate, reg6_strong, reg6_leader, type = "latex", style="jpam", column.labels=c("Modest","Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_2_1.tex"
stargazer(reg6_sub1, reg6_strong, reg6_leader, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_3_1.tex"
stargazer(reg6_sub1, reg6_sub2, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong + Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

# Log Employment based
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "$Log(Employment)$")
out_file <- "MeanTable15_2_classcomparison_1_2.tex"
stargazer(reg7_modest, reg7_moderate, reg7_strong, reg7_leader, type = "latex", style="jpam", column.labels=c("Modest","Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_2_2.tex"
stargazer(reg7_sub1, reg7_strong, reg7_leader, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_3_2.tex"
stargazer(reg7_sub1, reg7_sub2, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong + Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)


# SME + Log Employment Based
reg_cov_labels <- c("Patent Only", "TM Only", "Design Only", "Patent - TM", "Patent - Design", "TM - Design", "Patent - TM - Design", "Age", "SME", "$Log(Employment)$")
out_file <- "MeanTable15_2_classcomparison_1_3.tex"
stargazer(reg8_modest, reg8_moderate, reg8_strong, reg8_leader, type = "latex", style="jpam", column.labels=c("Modest","Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_2_3.tex"
stargazer(reg8_sub1, reg8_strong, reg8_leader, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong","Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)

out_file <- "MeanTable15_2_classcomparison_3_3.tex"
stargazer(reg8_sub1, reg8_sub2, type = "latex", style="jpam", column.labels=c("Modest + Moderate","Strong + Leader"), dep.var.labels = "$log (Rev / Employee)$", covariate.labels=reg_cov_labels, omit = c("country","sector"), omit.labels = c("Country?","Sector?"), title="Table 15 Model 2 - Classes Comparison 1", align=FALSE, font.size = "tiny", float = FALSE, single.row = FALSE, keep.stat = c("n","rsq","adj.rsq","res.dev","aic", "bic"), out=out_file)


