rm(list=ls())

#Libraries
library(readr)
library(scales)
library(ggplot2)
library(dplyr)
library(psych)   #install.packages("psych")
library(ggpubr)  #install.packages("ggpubr")
library(plotly)  #install.packages("plotly")


#First Data: WHO-COVID-19-global-data
df1 <- read_csv("WHO-COVID-19-global-data.csv", col_types = cols(New_cases = col_skip(),
                                                                 New_deaths = col_skip(),
                                                                 Country_code = col_skip()))
df1 <- subset(df1, Date_reported == as.Date("2020-10-15"))
df1$WHO_region <- factor(df1$WHO_region)


#Imputing ISO country codes into df1
url <- "https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv"
download.file(url, "ISO_code.csv")
ISO <- read_csv("ISO_code.csv", col_names = TRUE)

ISO <- ISO %>% 
  rename(Country = COUNTRY, `GDP(billions)` = `GDP (BILLIONS)`, ISO_Code = CODE) %>% 
  mutate(Country = recode(Country,
                          "Bahamas, The" = "Bahamas",
                          "Bolivia" = "Bolivia (Plurinational State of)",
                          "Brunei" = "Brunei Darussalam",
                          "Congo, Republic of the" = "Congo",
                          "Cote d'Ivoire" = "Côte d’Ivoire",
                          "Curacao" = "Curaçao",
                          "Czech Republic" = "Czechia",
                          "Falkland Islands (Islas Malvinas)" = "Falkland Islands (Malvinas)",
                          "Gambia, The" = "Gambia",
                          "Iran" = "Iran (Islamic Republic of)",
                          "Korea, North" = "Democratic People's Republic of Korea",
                          "Korea, South" = "Republic of Korea",
                          "Kosovo" = "Kosovo[1]",
                          "Laos" = "Lao People's Democratic Republic",
                          "Micronesia, Federated States of" = "Micronesia (Federated States of)",
                          "Northern Mariana Islands" = "Northern Mariana Islands (Commonwealth of the)",
                          "Russia" = "Russian Federation",
                          "Moldova" = "Republic of Moldova",
                          "Syria" = "Syrian Arab Republic",
                          "United Kingdom" = "The United Kingdom",
                          "Tanzania" = "United Republic of Tanzania",
                          "United States" = "United States of America",
                          "Virgin Islands" = "United States Virgin Islands",
                          "Venezuela" = "Venezuela (Bolivarian Republic of)",
                          "Vietnam" = "Viet Nam"))


df1 <- merge(df1, ISO, by.x="Country", by.y="Country", all.x=TRUE, all.y=TRUE)
df1 <- df1[,c(1,2,7,3,4,5,6)]


#Second Data: WHO environmental factors
df2 <- read_csv("who_complete(R.version).csv")
df2$Country <- factor(df2$Country)


#Merge df1 and df2
df_merged <- merge(x = df1, y = df2, by = "Country", all = TRUE)
df_merged <- df_merged[complete.cases(df_merged), ]


#Checking correlation between cumulative_death and the other numerical variables
cor <- corr.test(df_merged[5:ncol(df_merged)])
cor


#Scatter Plot (GDP VS CumulativeDeath)
gdp <- ggscatter(df_merged, x = "GDP(billions)", y = "Cumulative_deaths",
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "GDP", ylab = "Cumulative Deaths")
gdp <- gdp + scale_x_continuous(name = "GDP",
                                    limits = c(0,3000)) # Setting the limit of "x" axis
gdp <- gdp + scale_y_continuous(name = "Cumulative Deaths",
                                    limits = c(0,50000)) # Setting the limit of "y" axis
gdp


#Scatter Plot (BasicSanitation VS CleanWater) + Cumulative Death
sani_water <- qplot(BasicSanitation,  # The "x" variable
                    CleanWater,  # The "y" variable
                    data = df_merged,  # Our data frame
                    geom = "point",  # The geometry for a scatter plot
                    color = Cumulative_deaths)   
sani_water

#Scatter Plot (HealthRisk VS CumulativeDeath)
health <- ggscatter(df_merged, x = "HealthRisks", y = "Cumulative_deaths",
                        add = "reg.line", conf.int = TRUE,
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Health Risk", ylab = "Cumulative Deaths (log)")
health <- ggpar(health, yscale = "log2")
health


#Scatter Plot (PopOver60 VS Cumulative_Death)
pop60 <- ggscatter(df_merged, x = "PopOver60", y = "Cumulative_deaths",
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Population over 60", ylab = "Cumulative Deaths")

pop60 <- pop60 + scale_x_continuous(name = "Population over 60",
                                    limits = c(0,100)) # Setting the limit of "x" axis
pop60 <- pop60 + scale_y_continuous(name = "Cumulative Deaths",
                                    limits = c(0,40000)) # Setting the limit of "y" axis

pop60


#Box plot (WHO_region VS Cumulative death)
qplot(WHO_region, Cumulative_deaths, data = df_merged, geom = "boxplot", log = "y")


#World map (Cumulative death)
death_wm <- plot_ly(df_merged, type='choropleth', 
               locations=df_merged$ISO_Code, 
               z=df_merged$Cumulative_deaths, 
               text=df_merged$Country,
               color = I("red"))

death_wm

