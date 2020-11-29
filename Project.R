rm(list=ls())

#Libraries
library(readr)
library(scales)
library(ggplot2)
library(psych)  #install.packages("psych")
library(ggpubr)  #install.packages("ggpubr")


#First Data: WHO-COVID-19-global-data
df1 <- read_csv("WHO-COVID-19-global-data.csv", col_types = cols(New_cases = col_skip(),
                                                                 New_deaths = col_skip()))
df1 <- subset(df1, Date_reported == as.Date("2020-10-15"))
df1$WHO_region <- factor(df1$WHO_region)
df1$Country_code <- factor(df1$Country_code)


#Second Data:
df2 <- read_csv("who_complete(R.version).csv")
df2$Country <- factor(df2$Country)


#Merge df1 and df2
df_merged <- merge(x = df1, y = df2, by = "Country", all = TRUE)
df_merged <- df_merged[complete.cases(df_merged), ]


#Checking correlation between cumulative_death and the other numerical variables
corr.test(df_merged[5:ncol(df_merged)])


#Scatter Plot (BasicSanitation VS CleanWater) + Cumulative Death
sani_water <- qplot(BasicSanitation,  # The "x" variable
                    CleanWater,  # The "y" variable
                    data = df_merged,  # Our data frame
                    geom = "point",  # The geometry for a scatter plot
                    color = Cumulative_deaths)   


#Scatter Plot (HealthRisk VS CumulativeDeath)
healthplot <- ggscatter(df_merged, x = "HealthRisks", y = "Cumulative_deaths",
                        add = "reg.line", conf.int = TRUE,
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Health Risk", ylab = "Cumulative Deaths (log)")
healthplot <- ggpar(healthplot,yscale = "log2")
healthplot


#PopOver60 VS Cumulative_Death & Cumulative Cases
pop60 <- ggscatter(df_merged, x = "PopOver60", y = "Cumulative_deaths",
                   add = "reg.line", conf.int = TRUE,
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "Population over 60", ylab = "Cumulative Deaths")

pop60 <- pop60 + scale_x_continuous(name = "Population over 60",
                                    limits = c(0,100)) # Settomg the limit of "x" axis
pop60 <- pop60 + scale_y_continuous(name = "Cumulative Deaths",
                                    limits = c(0,40000)) # Settomg the limit of "y" axis

pop60

#WHO_region VS Cumulative
qplot(WHO_region, Cumulative_deaths, data = df_merged, geom = "boxplot", log = "y")


#
