# Load libraries
library(eulerr)
library(dplyr)
library(modelsummary)
library(kableExtra)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(broom)


# Define Working directory
setwd("C:/Users/MG53060/Documents/GitHub/EuropeEconomics/Figures")
# Read processed dataset
Data <- readRDS(file = "./CleanData.rds")

# Venn Diagrams

# Size based decomposition (Full Sample - SMEs - Large companies)
venn_data <- Data
venn_tot <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                    "B" = nrow(subset(venn_data, tm_only == 1)), 
                    "C" = nrow(subset(venn_data, des_only == 1)), 
                    "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                    "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                    "B&C" = nrow(subset(venn_data, tm_des == 1)),
                    "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                  shape = "circle")

venn_data <- filter(Data, sme == 'sme')
venn_sme <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                    "B" = nrow(subset(venn_data, tm_only == 1)), 
                    "C" = nrow(subset(venn_data, des_only == 1)), 
                    "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                    "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                    "B&C" = nrow(subset(venn_data, tm_des == 1)),
                    "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                  shape = "circle")

venn_data <- filter(Data, sme == 'large')
venn_large <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                      "B" = nrow(subset(venn_data, tm_only == 1)), 
                      "C" = nrow(subset(venn_data, des_only == 1)), 
                      "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                      "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                      "B&C" = nrow(subset(venn_data, tm_des == 1)),
                      "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                    shape = "circle")


# Save venn diagrams to wmf (Windows Meta File)
plot_fontsize = 22
plot_legendfontsize = 12


win.metafile(file = "VennDiagram_Total.wmf")
p <- plot(venn_tot,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("  Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

win.metafile(file = "VennDiagram_SME.wmf")
p <- plot(venn_sme,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("  Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

win.metafile(file = "VennDiagram_Large.wmf")
p <- plot(venn_large,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("     Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

# Geographically based decomposition (Full Sample - Modest+Moderate - Strong+Leader)

venn_data <- Data
venn_tot <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                    "B" = nrow(subset(venn_data, tm_only == 1)), 
                    "C" = nrow(subset(venn_data, des_only == 1)), 
                    "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                    "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                    "B&C" = nrow(subset(venn_data, tm_des == 1)),
                    "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                  shape = "circle")

venn_data <- filter(Data, innovationLevel == 'modest' |  innovationLevel == 'moderate')
venn_mm <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                   "B" = nrow(subset(venn_data, tm_only == 1)), 
                   "C" = nrow(subset(venn_data, des_only == 1)), 
                   "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                   "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                   "B&C" = nrow(subset(venn_data, tm_des == 1)),
                   "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                 shape = "circle")

venn_data <- filter(Data, innovationLevel == 'strong' |  innovationLevel == 'leader')
venn_sl <- euler(c("A" = nrow(subset(venn_data, patent_only == 1)), 
                   "B" = nrow(subset(venn_data, tm_only == 1)), 
                   "C" = nrow(subset(venn_data, des_only == 1)), 
                   "A&B" = nrow(subset(venn_data, pat_tm == 1  )), 
                   "A&C" = nrow(subset(venn_data, pat_des == 1)), 
                   "B&C" = nrow(subset(venn_data, tm_des == 1)),
                   "A&B&C" = nrow(subset(venn_data, pat_tm_des == 1))),
                 shape = "circle")

# Save venn diagrams to wmf (Windows Meta File)

win.metafile(file = "VennDiagram_Total.wmf")
p <- plot(venn_tot,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("  Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

win.metafile(file = "VennDiagram_MM.wmf")
p <- plot(venn_mm,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("  Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

win.metafile(file = "VennDiagram_SL.wmf")
p <- plot(venn_sl,
     fills = list(fill = c("salmon", "slategray1","yellowgreen")),
     edges = list(col = "white", lex = 4),
     quantities = list(font = 4, fontsize = plot_fontsize,type = c("percent")),
     labels = list(labels =c("     Patent ","TM","Design"), fontsize = plot_fontsize, font = 2)                            
     #legend = list(labels =c("Patent","TM","Design"),fontsize = plot_legendfontsize,font = 2,side = "right")
)
print(p)
dev.off()

# Perfom output of the Regression tables

# Model 1 - Full Sample - Model Construction

# Tranform SME into factor
Data$sme <- as.factor(Data$sme)

# Initialize models list
model1 <- list()

model1[['Variant 1']] <- lm(log_rev_empl ~ ip_owner + age                                              , data = Data)
model1[['Variant 2']] <- lm(log_rev_empl ~ ip_owner + age + country                                    , data = Data)
model1[['Variant 3']] <- lm(log_rev_empl ~ ip_owner + age +         + sector                           , data = Data)
model1[['Variant 4']] <- lm(log_rev_empl ~ ip_owner + age + country + sector                           , data = Data)
model1[['Variant 5']] <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme                     , data = Data)
model1[['Variant 6']] <- lm(log_rev_empl ~ ip_owner + age + country + sector +     + log(employment+1) , data = Data)
model1[['Variant 7']] <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = Data)

cm <- c( "ip_owner" = "IP Ownership", "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')
cap <- '<b> Model 1: IP Ownership </b>'
row1 <- c('Country', 'No', 'Yes', 'No', 'Yes', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector', 'No', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
row.names(df_rows) <- NULL


tab <- msummary(model1,
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
  column_spec(8, color = "deepskyblue") %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(5, extra_css = "border-bottom: 2px solid") %>%
  row_spec(7, extra_css = "border-bottom: 2px solid") %>%
  row_spec(9, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model1_Overview.jpg", zoom = 2.0)


# Model 1 - Size Based Split

Data_sme   <- filter(Data, sme == 'sme')
Data_large <- filter(Data, sme == 'large')

# Define models list
model1_size <- list()
model1_size[['Full Sample']] <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = Data)
model1_size[['SMEs']]        <- lm(log_rev_empl ~ ip_owner + age + country + sector       + log(employment+1) , data = Data_sme)
model1_size[['Large']]       <- lm(log_rev_empl ~ ip_owner + age + country + sector       + log(employment+1) , data = Data_large)

cm <- c( "ip_owner" = "IP Ownership", "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')
cap <- '<b> Model 1: IP Ownership split between SMEs and large companies </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector' , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
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
  row_spec(9, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model1_SizeSplit_Overview.jpg", zoom = 2.0)

# Model 1 geographical split

Data_MM <- filter(Data, innovationLevel == 'modest' |  innovationLevel == 'moderate')
Data_SL <- filter(Data, innovationLevel == 'strong' |  innovationLevel == 'leader')

# Define models list
model1_geo <- list()
model1_geo[["Full Sample"]]       <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = Data)
model1_geo[["Modest+Moderate"]] <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = Data_MM)
model1_geo[["Strong+Leading"]]  <- lm(log_rev_empl ~ ip_owner + age + country + sector + sme + log(employment+1) , data = Data_SL)

cm <- c( "ip_owner" = "IP Ownership", "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')
cap <- '<b> Model 1: IP Ownership split based on European Innovation Scoreboard </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector' , 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
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
  row_spec(9, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model1_GeoSplit_Overview.jpg", zoom = 2.0)

# Model 2 - Full Sample - Model Construction

model2 <- list()

model2[['Variant 1']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age                    , data = Data)
model2[['Variant 2']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country          , data = Data)
model2[['Variant 3']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age           + sector , data = Data)
model2[['Variant 4']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector , data = Data)
model2[['Variant 5']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme , data = Data)
model2[['Variant 6']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = Data)
model2[['Variant 7']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = Data)

cm <- c( "patent_only" = "Patent only (b1)" ,
         "tm_only" = "Trademark Only (b2)",
         "des_only" = "Design only (b3)",
         "pat_tm" = "Patent - TM (b4)",
         "pat_des" = "Patent - Design (b5)",
         "tm_des" = "TM - Design (b6)",
         "pat_tm_des" = "Patent - TM - Design (b7)",
         "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')

cap <- '<b> Model 2: IP Strategies </b>'
row1 <- c('Country', 'No', 'Yes', 'No', 'Yes', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector', 'No', 'No', 'Yes', 'Yes', 'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
row.names(df_rows) <- NULL


tab <- msummary(model2,
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
  column_spec(8, color = "deepskyblue") %>%
  row_spec(0, extra_css = "border-bottom: 2px solid") %>%
  row_spec(11, extra_css = "border-bottom: 2px solid") %>%
  row_spec(13, extra_css = "border-bottom: 2px solid") %>%
  row_spec(15, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model2_Overview.jpg", zoom = 2.0)

# Model 2 - Size Based Split

model2_size <- list()

model2_size[['Full Sample']] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = Data)
model2_size[['SMEs']]        <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = Data_sme)
model2_size[['Large']]       <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector       + log(employment+1) , data = Data_large)

cm <- c( "patent_only" = "Patent only (b1)" ,
         "tm_only" = "Trademark Only (b2)",
         "des_only" = "Design only (b3)",
         "pat_tm" = "Patent - TM (b4)",
         "pat_des" = "Patent - Design (b5)",
         "tm_des" = "TM - Design (b6)",
         "pat_tm_des" = "Patent - TM - Design (b7)",
         "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')

cap <- '<b> Model 2: IP Strategies split between SMEs and large companies </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector',  'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
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
  row_spec(15, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model2_SizeSplit_Overview.jpg", zoom = 2.0)

# Model 2 Geographical based split

model2_geo <- list()

model2_geo[["Full Sample"]]       <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = Data)
model2_geo[["Modest+Moderate"]] <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = Data_MM)
model2_geo[["Strong+Leading"]]  <- lm(log_rev_empl ~ patent_only + tm_only + des_only + pat_tm + pat_des + tm_des + pat_tm_des + age + country + sector + sme + log(employment+1) , data = Data_SL)

cm <- c( "patent_only" = "Patent only (b1)" ,
         "tm_only" = "Trademark Only (b2)",
         "des_only" = "Design only (b3)",
         "pat_tm" = "Patent - TM (b4)",
         "pat_des" = "Patent - Design (b5)",
         "tm_des" = "TM - Design (b6)",
         "pat_tm_des" = "Patent - TM - Design (b7)",
         "age" = "Age", "smesme" = "SME", "log(employment + 1)" = "Log(Employment + 1)", '(Intercept)' = 'Constant')

cap <- '<b> Model 2: IP Strategies split based on European Innovation Scoreboard </b>'
row1 <- c('Country', 'Yes', 'Yes', 'Yes')
row2 <- c('Sector',  'Yes', 'Yes', 'Yes')
df_rows <- data.frame(rbind(row1, row2))
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
  row_spec(15, extra_css = "border-bottom: 2px solid") %>%
  save_kable("./Model2_GeoSplit_Overview.jpg", zoom = 2.0)

##################################################
# European Innovation Scoreboard geographical map

worldmap <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
europe_cropped <- st_crop(worldmap, xmin = -10, xmax = 45, ymin = 33, ymax = 72)

# Create example data frame
geodata <- structure(list(Country = c("Romania", "Bulgaria",
                                   "Croatia", "Poland", "Latvia", "Hungary", "Slovakia", "Estonia", "Lithuania",
                                   "Italy", "Malta" , "Czech Republic", "Slovenia", "Spain", "Cyprus", "Greece",
                                   "Portugal", "France", "Ireland", "Austria", "Germany", "Belgium", "United Kingdom",
                                   "Luxembourg", "Netherlands", "Denmark", "Finland", "Sweden"
),
InnovationLevel = c("Modest","Modest",
                    "Moderate", "Moderate", "Moderate", "Moderate", "Moderate", "Moderate", "Moderate",
                    "Moderate", "Moderate" , "Moderate", "Moderate", "Moderate", "Moderate", "Moderate",
                    "Strong", "Strong", "Strong", "Strong", "Strong", "Strong", "Strong",
                    "Leader", "Leader", "Leader", "Leader", "Leader"
)
),
row.names=1:28, class = "data.frame")

geodata$InnovationLevel <- as.factor(geodata$InnovationLevel)
geodata$InnovationLevel <- factor(geodata$InnovationLevel, levels = c("Modest", "Moderate", "Strong", "Leader"))

# Merge data to state and filter for these records
state_filter <- europe_cropped %>% 
  left_join(geodata, by = c("sovereignt" = "Country")) 

state_filter_sp <- as(state_filter, "Spatial")


ggplot() +
  annotation_spatial(state_filter_sp) +
  layer_spatial(state_filter_sp, aes(fill = InnovationLevel)) +
  ggtitle("European Innovation Scoreboard", subtitle = paste0("(", length(unique(geodata$Country)), " countries)")) +
  theme_minimal() +
  theme(
    text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position = "right",
    legend.title = element_blank()
  ) +
  scale_fill_manual( breaks = c("Modest", "Moderate", "Strong", "Leader"),
                     values = scales::hue_pal()(4) )

ggsave("InnovationLevelMap.jpg",  dpi = 900)

##############################
# Barplot Model 1 - Size Split

# Create dataframe
coeff_whole_m1_size <- model1_size[['Full Sample']]$coefficients[2]
coeff_sme_m1_size   <- model1_size[['SMEs']]$coefficients[2]
coeff_large_m1_size <- model1_size[['Large']]$coefficients[2]

perc_whole_m1_size <- 100.0 * (exp(coeff_whole_m1_size) - 1.0)
perc_sme_m1_size   <- 100.0 * (exp(coeff_sme_m1_size)   - 1.0)
perc_large_m1_size <- 100.0 * (exp(coeff_large_m1_size) - 1.0)

df_model1_size <- data.frame(coeff = c(coeff_whole_m1_size, coeff_sme_m1_size, coeff_large_m1_size),
                             perc  = c(perc_whole_m1_size , perc_sme_m1_size , perc_large_m1_size ),
                             group = c("Full Sample", "SMEs", "Large"))

df_model1_size$group <- factor(df_model1_size$group, levels = c("Full Sample", "SMEs", "Large"))

df_model1_size %>% 
  ggplot(aes(group, perc)) +
  geom_col(aes(fill = group), color = "black", width = 0.85) +
  scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4")) +
  annotate("text", x = 1  , y = 27  , label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 2  , y = 29  , label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 3  , y = 13.5, label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 1  , y = 5.0 , label = paste0("N = ", nrow(Data)      ), size = 6, color = "white", angle = 90) +
  annotate("text", x = 2  , y = 5.0 , label = paste0("N = ", nrow(Data_sme)  ), size = 6, color = "white", angle = 90) +
  annotate("text", x = 3  , y = 5.0 , label = paste0("N = ", nrow(Data_large)), size = 6, color = "white", angle = 90) +
  scale_y_continuous(limits = c(0, 36), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Dataset",
    y = "% Difference",
    title = "Model 1: Log-Linear Regression Coefficients - Size Split"
  ) + 
  theme(
    aspect.ratio = 1, 
    plot.title = element_text(size = 10,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 14, color = "#22292F"),
    axis.title = element_text(size = 14, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
    
  ) 

ggsave("BarPlot-Model1-SizeComparison.jpg",  dpi = 900)

######################################
# Barplot Model 1 - Geographical Split

# Create dataframe
coeff_whole_m1_geo <- model1_geo[["Full Sample"]]$coefficients[2]
coeff_sme_m1_geo   <- model1_geo[["Modest+Moderate"]]$coefficients[2]
coeff_large_m1_geo <- model1_geo[["Strong+Leading"]]$coefficients[2]

perc_whole_m1_geo <- 100.0 * (exp(coeff_whole_m1_geo) - 1.0)
perc_sme_m1_geo   <- 100.0 * (exp(coeff_sme_m1_geo)   - 1.0)
perc_large_m1_geo <- 100.0 * (exp(coeff_large_m1_geo) - 1.0)

df_model1_geo <- data.frame(coeff = c(coeff_whole_m1_geo, coeff_sme_m1_geo, coeff_large_m1_geo),
                            perc  = c(perc_whole_m1_geo , perc_sme_m1_geo , perc_large_m1_geo ),
                            group = c("Full Sample", "Modest+Moderate", "Strong+Leading"))

df_model1_geo$group <- factor(df_model1_geo$group, levels = c("Full Sample", "Modest+Moderate", "Strong+Leading"))

df_model1_geo %>% 
  ggplot(aes(group, perc)) +
  geom_col(aes(fill = group), color = "black", width = 0.85) +
  scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4")) +
  annotate("text", x = 1  , y = 27  , label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 2  , y = 33  , label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 3  , y = 21  , label = "***", size = 8, color = "#22292F") +
  annotate("text", x = 1  , y = 5.0 , label = paste0("N = ", nrow(Data)   ), size = 6, color = "white", angle = 90) +
  annotate("text", x = 2  , y = 5.0 , label = paste0("N = ", nrow(Data_MM)), size = 6, color = "white", angle = 90) +
  annotate("text", x = 3  , y = 5.0 , label = paste0("N = ", nrow(Data_SL)), size = 6, color = "white", angle = 90) +
  scale_y_continuous(limits = c(0, 36), expand = c(0, 0)) +
  guides(fill = FALSE) +
  theme_minimal() +
  labs(
    x = "Dataset",
    y = "% Difference",
    title = "Model 1: Log-Linear Regression Coefficients - Geographical Split"
  ) + 
  theme(
    aspect.ratio = 1,
    plot.title = element_text(size = 10,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 14, color = "#22292F"),
    axis.title = element_text(size = 14, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
    
  ) 

ggsave("BarPlot-Model1-GeoComparison.jpg",  dpi = 900)

######################################
# Barplot Model 2 - Size Split

coeffs_whole_m2_size <- model2_size[['Full Sample']]$coefficients[2:8]
coeffs_sme_m2_size   <- model2_size[['SMEs']]$coefficients[2:8]
coeffs_large_m2_size <- model2_size[['Large']]$coefficients[2:8]

perc_whole_m2_size <- 100.0 * (exp(coeffs_whole_m2_size) - 1.0)
perc_sme_m2_size   <- 100.0 * (exp(coeffs_sme_m2_size) - 1.0)
perc_large_m2_size <- 100.0 * (exp(coeffs_large_m2_size) - 1.0)

cutpoints_vec <- c(0, 0.01, 0.05, 0.1, 1.0)
symbols_vec   <- c("***", "**", "*", " ")

label_full  <- symnum(tidy(model2_size[['Full Sample']])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]
label_sme   <- symnum(tidy(model2_size[['SMEs']])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]
label_large <- symnum(tidy(model2_size[['Large']])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]

# Add sub sample size
label_full[1] <- paste0(label_full[1], " N = ", nrow(subset(Data, patent_only == 1)))
label_full[2] <- paste0(label_full[2], " N = ", nrow(subset(Data, tm_only     == 1)))
label_full[3] <- paste0(label_full[3], " N = ", nrow(subset(Data, des_only    == 1)))
label_full[4] <- paste0(label_full[4], " N = ", nrow(subset(Data, pat_tm      == 1)))
label_full[5] <- paste0(label_full[5], " N = ", nrow(subset(Data, pat_des     == 1)))
label_full[6] <- paste0(label_full[6], " N = ", nrow(subset(Data, tm_des      == 1)))
label_full[7] <- paste0(label_full[7], " N = ", nrow(subset(Data, pat_tm_des  == 1)))

label_sme[1] <- paste0(label_sme[1], " N = ", nrow(subset(Data_sme, patent_only == 1)))
label_sme[2] <- paste0(label_sme[2], " N = ", nrow(subset(Data_sme, tm_only     == 1)))
label_sme[3] <- paste0(label_sme[3], " N = ", nrow(subset(Data_sme, des_only    == 1)))
label_sme[4] <- paste0(label_sme[4], " N = ", nrow(subset(Data_sme, pat_tm      == 1)))
label_sme[5] <- paste0(label_sme[5], " N = ", nrow(subset(Data_sme, pat_des     == 1)))
label_sme[6] <- paste0(label_sme[6], " N = ", nrow(subset(Data_sme, tm_des      == 1)))
label_sme[7] <- paste0(label_sme[7], " N = ", nrow(subset(Data_sme, pat_tm_des  == 1)))

label_large[1] <- paste0(label_large[1], " N = ", nrow(subset(Data_large, patent_only == 1)))
label_large[2] <- paste0(label_large[2], " N = ", nrow(subset(Data_large, tm_only     == 1)))
label_large[3] <- paste0(label_large[3], " N = ", nrow(subset(Data_large, des_only    == 1)))
label_large[4] <- paste0(label_large[4], " N = ", nrow(subset(Data_large, pat_tm      == 1)))
label_large[5] <- paste0(label_large[5], " N = ", nrow(subset(Data_large, pat_des     == 1)))
label_large[6] <- paste0(label_large[6], " N = ", nrow(subset(Data_large, tm_des      == 1)))
label_large[7] <- paste0(label_large[7], " N = ", nrow(subset(Data_large, pat_tm_des  == 1)))

# Whole Dataset
df_m2_size_fs <- data.frame(coeff = coeffs_whole_m2_size,
                            perc  = perc_whole_m2_size,
                            group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_size_fs$dataset <- "Full Sample"

# SMEs
df_m2_size_sme <- data.frame(coeff = coeffs_sme_m2_size,
                             perc  = perc_sme_m2_size,
                             group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_size_sme$dataset <- "SMEs"

# Large companies
df_m2_size_large <- data.frame(coeff = coeffs_large_m2_size,
                               perc  = perc_large_m2_size,
                               group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_size_large$dataset <- "Large"


df_m2_size <- rbind(df_m2_size_fs, df_m2_size_sme, df_m2_size_large)
df_m2_size$dataset <- factor(df_m2_size$dataset, levels = c("Full Sample", "SMEs", "Large"))

offset_x1 <- 0.22
offset_x2 <- 0.0
offset_x3 <- 0.25
offset_y1  <- c(5.05, 6.05, 5.35, 5.65, 5.35, 5.65, 5.65)
offset_y2  <- c(5.05, 6.05, 5.35, 5.65, 5.35, 5.35, 5.35)
offset_y3  <- c(5.05, 5.75, 5.35, 5.35, 5.35, 5.35, 5.35)
label_fs  <- 2.5

ggplot(data=df_m2_size, aes(x=group, y=perc, fill=dataset)) +
  geom_col(colour="black",width=0.6,position=position_dodge(0.6)) +
  scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4")) +
  scale_y_continuous(limits = c(0, 45), expand = c(0, 0)) +
  annotate("text", x = (1.0 - offset_x1), y = perc_whole_m2_size[1] + offset_y1[1], label = label_full[1], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (1.0 + offset_x2), y = perc_sme_m2_size[1]   + offset_y2[1], label = label_sme[1]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (1.0 + offset_x3), y = perc_large_m2_size[1] + offset_y3[1], label = label_large[1], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 - offset_x1), y = perc_whole_m2_size[2] + offset_y1[2], label = label_full[2], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 + offset_x2), y = perc_sme_m2_size[2]   + offset_y2[2], label = label_sme[2]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 + offset_x3), y = perc_large_m2_size[2] + offset_y3[2], label = label_large[2], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 - offset_x1), y = perc_whole_m2_size[3] + offset_y1[3], label = label_full[3], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 + offset_x2), y = perc_sme_m2_size[3]   + offset_y2[3], label = label_sme[3]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 + offset_x3), y = perc_large_m2_size[3] + offset_y3[3], label = label_large[3], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 - offset_x1), y = perc_whole_m2_size[4] + offset_y1[4], label = label_full[4], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 + offset_x2), y = perc_sme_m2_size[4]   + offset_y2[4], label = label_sme[4]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 + offset_x3), y = perc_large_m2_size[4] + offset_y3[4], label = label_large[4], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 - offset_x1), y = perc_whole_m2_size[5] + offset_y1[5], label = label_full[5], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 + offset_x2), y = perc_sme_m2_size[5]   + offset_y2[5], label = label_sme[5]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 + offset_x3), y = perc_large_m2_size[5] + offset_y3[5], label = label_large[5], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 - offset_x1), y = perc_whole_m2_size[6] + offset_y1[6], label = label_full[6], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 + offset_x2), y = perc_sme_m2_size[6]   + offset_y2[6], label = label_sme[6]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 + offset_x3), y = perc_large_m2_size[6] + offset_y3[6], label = label_large[6], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 - offset_x1), y = perc_whole_m2_size[7] + offset_y1[7], label = label_full[7], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 + offset_x2), y = perc_sme_m2_size[7]   + offset_y2[7], label = label_sme[7]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 + offset_x3), y = perc_large_m2_size[7] + offset_y3[7], label = label_large[7], size = label_fs, color = "#22292F", angle = 90) +
  theme_minimal() +
  labs(
    x = "Coefficient",
    y = "% Difference",
    title = "Model 2: Log-Linear Regression Coefficients - Size Split"
  ) +
  theme(
    aspect.ratio = 3/5,  
    plot.title = element_text(size = 10,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 14, color = "#22292F"),
    axis.title = element_text(size = 14, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",  
    legend.title = element_blank()  
  )

ggsave("BarPlot-Model2-SizeComparison.jpg",  dpi = 900)


######################################
# Barplot Model 2 - Geo Split

coeffs_whole_m2_geo <- model2_geo[["Full Sample"]]$coefficients[2:8]
coeffs_mm_m2_geo    <- model2_geo[["Modest+Moderate"]]$coefficients[2:8]
coeffs_sl_m2_geo    <- model2_geo[["Strong+Leading"]]$coefficients[2:8]

perc_whole_m2_geo <- 100.0 * (exp(coeffs_whole_m2_geo) - 1.0)
perc_mm_m2_geo    <- 100.0 * (exp(coeffs_mm_m2_geo) - 1.0)
perc_sl_m2_geo    <- 100.0 * (exp(coeffs_sl_m2_geo) - 1.0)

cutpoints_vec <- c(0, 0.01, 0.05, 0.1, 1.0)
symbols_vec   <- c("***", "**", "*", " ")

label_full <- symnum(tidy(model2_geo[["Full Sample"]])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]
label_MM   <- symnum(tidy(model2_geo[["Modest+Moderate"]])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]
label_SL   <- symnum(tidy(model2_geo[["Strong+Leading"]])$p.value, corr = FALSE, na = FALSE, cutpoints = cutpoints_vec, symbols = symbols_vec)[2:8]

# Add sub sample size
label_full[1] <- paste0(label_full[1], " N = ", nrow(subset(Data, patent_only == 1)))
label_full[2] <- paste0(label_full[2], " N = ", nrow(subset(Data, tm_only     == 1)))
label_full[3] <- paste0(label_full[3], " N = ", nrow(subset(Data, des_only    == 1)))
label_full[4] <- paste0(label_full[4], " N = ", nrow(subset(Data, pat_tm      == 1)))
label_full[5] <- paste0(label_full[5], " N = ", nrow(subset(Data, pat_des     == 1)))
label_full[6] <- paste0(label_full[6], " N = ", nrow(subset(Data, tm_des      == 1)))
label_full[7] <- paste0(label_full[7], " N = ", nrow(subset(Data, pat_tm_des  == 1)))

label_MM[1] <- paste0(label_MM[1], " N = ", nrow(subset(Data_MM, patent_only == 1)))
label_MM[2] <- paste0(label_MM[2], " N = ", nrow(subset(Data_MM, tm_only     == 1)))
label_MM[3] <- paste0(label_MM[3], " N = ", nrow(subset(Data_MM, des_only    == 1)))
label_MM[4] <- paste0(label_MM[4], " N = ", nrow(subset(Data_MM, pat_tm      == 1)))
label_MM[5] <- paste0(label_MM[5], " N = ", nrow(subset(Data_MM, pat_des     == 1)))
label_MM[6] <- paste0(label_MM[6], " N = ", nrow(subset(Data_MM, tm_des      == 1)))
label_MM[7] <- paste0(label_MM[7], " N = ", nrow(subset(Data_MM, pat_tm_des  == 1)))

label_SL[1] <- paste0(label_SL[1], " N = ", nrow(subset(Data_SL, patent_only == 1)))
label_SL[2] <- paste0(label_SL[2], " N = ", nrow(subset(Data_SL, tm_only     == 1)))
label_SL[3] <- paste0(label_SL[3], " N = ", nrow(subset(Data_SL, des_only    == 1)))
label_SL[4] <- paste0(label_SL[4], " N = ", nrow(subset(Data_SL, pat_tm      == 1)))
label_SL[5] <- paste0(label_SL[5], " N = ", nrow(subset(Data_SL, pat_des     == 1)))
label_SL[6] <- paste0(label_SL[6], " N = ", nrow(subset(Data_SL, tm_des      == 1)))
label_SL[7] <- paste0(label_SL[7], " N = ", nrow(subset(Data_SL, pat_tm_des  == 1)))

# Whole Dataset
df_m2_geo_fs <- data.frame(coeff = coeffs_whole_m2_geo,
                           perc  = perc_whole_m2_geo,
                           group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_geo_fs$dataset <- "Full Sample"

# M+M
df_m2_geo_MM <- data.frame(coeff = coeffs_mm_m2_geo,
                           perc  = perc_mm_m2_geo,
                           group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_geo_MM$dataset <- "Modest+Moderate"

# Large companies
df_m2_geo_SL <- data.frame(coeff = coeffs_sl_m2_geo,
                           perc  = perc_sl_m2_geo,
                           group = c("b1", "b2", "b3", "b4", "b5", "b6", "b7"))

df_m2_geo_SL$dataset <- "Strong+Leading"


df_m2_geo <- rbind(df_m2_geo_fs, df_m2_geo_MM, df_m2_geo_SL)
df_m2_geo$dataset <- factor(df_m2_geo$dataset, levels = c("Full Sample", "Modest+Moderate", "Strong+Leading"))

offset_x1 <- 0.22
offset_x2 <- 0.0
offset_x3 <- 0.25
offset_y1  <- c(5.65, 6.75, 5.35, 6.25, 5.35, 6.35, 6.05)
offset_y2  <- c(5.65, 6.25, 5.35, 5.85, 5.35, 5.85, 5.85)
offset_y3  <- c(5.65, 6.25, 5.35, 6.25, 5.35, 5.85, 5.85)
label_fs  <- 2.5

ggplot(data=df_m2_geo, aes(x=group, y=perc, fill=dataset)) +
  geom_col(colour="black",width=0.6,position=position_dodge(0.6)) +
  scale_fill_manual(values=c("steelblue1", "steelblue3", "steelblue4")) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  annotate("text", x = (1.0 - offset_x1), y = perc_whole_m2_geo[1] + offset_y1[1], label = label_full[1], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (1.0 + offset_x2), y = perc_mm_m2_geo[1]    + offset_y2[1], label = label_MM[1]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (1.0 + offset_x3), y = perc_sl_m2_geo[1]    + offset_y3[1], label = label_SL[1]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 - offset_x1), y = perc_whole_m2_geo[2] + offset_y1[2], label = label_full[2], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 + offset_x2), y = perc_mm_m2_geo[2]    + offset_y2[2], label = label_MM[2]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (2.0 + offset_x3), y = perc_sl_m2_geo[2]    + offset_y3[2], label = label_SL[2]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 - offset_x1), y = perc_whole_m2_geo[3] + offset_y1[3], label = label_full[3], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 + offset_x2), y = perc_mm_m2_geo[3]    + offset_y2[3], label = label_MM[3]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (3.0 + offset_x3), y = perc_sl_m2_geo[3]    + offset_y3[3], label = label_SL[3]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 - offset_x1), y = perc_whole_m2_geo[4] + offset_y1[4], label = label_full[4], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 + offset_x2), y = perc_mm_m2_geo[4]    + offset_y2[4], label = label_MM[4]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (4.0 + offset_x3), y = perc_sl_m2_geo[4]    + offset_y3[4], label = label_SL[4]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 - offset_x1), y = perc_whole_m2_geo[5] + offset_y1[5], label = label_full[5], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 + offset_x2), y = perc_mm_m2_geo[5]    + offset_y2[5], label = label_MM[5]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (5.0 + offset_x3), y = perc_sl_m2_geo[5]    + offset_y3[5], label = label_SL[5]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 - offset_x1), y = perc_whole_m2_geo[6] + offset_y1[6], label = label_full[6], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 + offset_x2), y = perc_mm_m2_geo[6]    + offset_y2[6], label = label_MM[6]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (6.0 + offset_x3), y = perc_sl_m2_geo[6]    + offset_y3[6], label = label_SL[6]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 - offset_x1), y = perc_whole_m2_geo[7] + offset_y1[7], label = label_full[7], size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 + offset_x2), y = perc_mm_m2_geo[7]    + offset_y2[7], label = label_MM[7]  , size = label_fs, color = "#22292F", angle = 90) +
  annotate("text", x = (7.0 + offset_x3), y = perc_sl_m2_geo[7]    + offset_y3[7], label = label_SL[7]  , size = label_fs, color = "#22292F", angle = 90) +
  theme_minimal() +
  labs(
    x = "Coefficient",
    y = "% Difference",
    title = "Model 2: Log-Linear Regression Coefficients - Geo Split"
  ) +
  theme(
    aspect.ratio = 3/5,  
    plot.title = element_text(size = 10,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 14, color = "#22292F"),
    axis.title = element_text(size = 14, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.y = element_text(margin = margin(r = 5)),
    axis.text.x = element_text(margin = margin(t = 5)),
    plot.caption = element_text(size = 12, 
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)),
    axis.line = element_line(color = "#3D4852"),
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "right",  
    legend.title = element_blank()  
  )

ggsave("BarPlot-Model2-GeoComparison.jpg",  dpi = 900)