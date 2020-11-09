library(modelsummary)
library(kableExtra)
library(dplyr)

# Define local working directory
setwd("C:/Users/MG53060/Documents/GitHub/EuropeEconomics")

# Read dataset containing models
load("./RandomEffectModels.RData")


# Model 1: Prepare tables for size comparison
model1_size <- list()
model1_size[['Full Sample']] <- model1_re
model1_size[['SMEs']]        <- model1_re_sme
model1_size[['Large']]       <- model1_re_large

cm <- c( "factor(ip_owner)TRUE" = "IP Ownership", "age" = "Age", "factor(sme)sme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')
cap <- '<b> Random Effects - Model 1: IP Ownership split between SMEs and large companies </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector' , 'Yes', 'Yes', 'Yes')
row3 <- c('Year'   , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2,row3))
row.names(df_rows) <- NULL

tab <- msummary(model1_size,
                output = 'html',
                add_rows = df_rows,
                add_rows_location = 0,
                coef_map = cm, 
                statistic_vertical = FALSE,
                stars = TRUE,
                title = cap,
                gof_omit = 'IC|Log|Adj|F')

tab %>%
  kable_styling("striped", html_font = "Arial") %>%
  column_spec(1, bold = T) %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(5, extra_css = "border-bottom: 2px solid") %>%
  row_spec(7, extra_css = "border-bottom: 2px solid") %>%
  row_spec(10, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./RE_Model1_SizeSplit_Overview.jpg", zoom = 2.0)

# Model 1: Prepare tables for geographical comparison

model1_geo <- list()
model1_geo[["Full Sample"]]     <- model1_re
model1_geo[["Modest+Moderate"]] <- model1_re_MM
model1_geo[["Strong+Leading"]]  <- model1_re_SL
cm <- c( "factor(ip_owner)TRUE" = "IP Ownership", "age" = "Age", "factor(sme)sme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')
cap <- '<b> Random Effects - Model 1: IP Ownership split based on European Innovation Scoreboard </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector' , 'Yes', 'Yes', 'Yes')
row3 <- c('Year'   , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2,row3))
row.names(df_rows) <- NULL

tab <- msummary(model1_geo,
                output = 'html',
                add_rows = df_rows,
                add_rows_location = 0,
                coef_map = cm, 
                statistic_vertical = FALSE,
                stars = TRUE,
                title = cap,
                gof_omit = 'IC|Log|Adj|F')

tab %>%
  kable_styling("striped", html_font = "Arial") %>%
  column_spec(1, bold = T) %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(5, extra_css = "border-bottom: 2px solid") %>%
  row_spec(7, extra_css = "border-bottom: 2px solid") %>%
  row_spec(10, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./RE_Model1_GeoSplit_Overview.jpg", zoom = 2.0)

# Model 2 - Size Based Split

model2_size <- list()

model2_size[['Full Sample']] <- model2_re
model2_size[['SMEs']]        <- model2_re_sme
model2_size[['Large']]       <- model2_re_large

cm <- c( "patent_onlyTRUE" = "Patent only" ,
         "tm_onlyTRUE" = "Trademark Only",
         "des_onlyTRUE" = "Design only",
         "pat_tmTRUE" = "Patent - TM",
         "pat_desTRUE" = "Patent - Design",
         "tm_desTRUE" = "TM - Design",
         "pat_tm_desTRUE" = "Patent - TM - Design",
         "age" = "Age", "factor(sme)sme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')

cap <- '<b> Random Effects - Model 2: IP Strategies split between SMEs and large companies </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector',  'Yes', 'Yes', 'Yes')
row3 <- c('Year'   , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2, row3))
row.names(df_rows) <- NULL


tab <- msummary(model2_size,
                output = 'html',
                add_rows = df_rows,
                add_rows_location = 0,
                coef_map = cm, 
                statistic_vertical = FALSE,
                stars = TRUE,
                title = cap,
                gof_omit = 'IC|Log|Adj|F')

tab %>%
  kable_styling("striped", html_font = "Arial") %>%
  column_spec(1, bold = T) %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(11, extra_css = "border-bottom: 2px solid") %>%
  row_spec(13, extra_css = "border-bottom: 2px solid") %>%
  row_spec(16, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./RE_Model2_SizeSplit_Overview.jpg", zoom = 2.0)

# Model 2 Geographical based split

model2_geo <- list()

model2_geo[["Full Sample"]]     <- model2_re
model2_geo[["Modest+Moderate"]] <- model2_re_MM
model2_geo[["Strong+Leading"]]  <- model2_re_SL

cm <- c( "patent_onlyTRUE" = "Patent only" ,
         "tm_onlyTRUE" = "Trademark Only",
         "des_onlyTRUE" = "Design only",
         "pat_tmTRUE" = "Patent - TM",
         "pat_desTRUE" = "Patent - Design",
         "tm_desTRUE" = "TM - Design",
         "pat_tm_desTRUE" = "Patent - TM - Design",
         "age" = "Age", "factor(sme)sme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')

cap <- '<b> Random Effects - Model 2: IP Strategies split based on European Innovation Scoreboard </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row3 <- c('Year'   , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2, row3))
row.names(df_rows) <- NULL


tab <- msummary(model2_geo,
                output = 'html',
                add_rows = df_rows,
                add_rows_location = 0,
                coef_map = cm, 
                statistic_vertical = FALSE,
                stars = TRUE,
                title = cap,
                gof_omit = 'IC|Log|Adj|F')

tab %>%
  kable_styling("striped", html_font = "Arial") %>%
  column_spec(1, bold = T) %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(11, extra_css = "border-bottom: 2px solid") %>%
  row_spec(13, extra_css = "border-bottom: 2px solid") %>%
  row_spec(16, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./RE_Model2_GeoSplit_Overview.jpg", zoom = 2.0)

