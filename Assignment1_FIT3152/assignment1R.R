setwd("C:/Monash/FIT3152/Assignment1")
install.packages("dplyr")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages("patchwork")
install.packages("reshape2")
library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(factoextra)
library(tidyr)
library(reshape2)

cvbase = read.csv("PsyCoronaBaselineExtract.csv")
dim(cvbase)
rm(list = ls())
set.seed(31860532) # XXXXXXXX = your student ID
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows

#Q1 Descriptive analysis and pre-processing

#Q1a
#Dimension
dim(cvbase)
ncol(cvbase) #There are 54 columns
nrow(cvbase) #There are 40000 rows
str(cvbase) 

age_group = cvbase %>% group_by(age) %>% summarise(COUNT = n()) #count for the age groups
age_group <- as.data.frame(age_group, row.names = NULL, optional = FALSE)
age_group_bar = age_group$COUNT
names(age_group_bar) <- c("18-25", "25-34", "35-44", "45-54", "55-64", "65-75", "75-85", "85+", "NA")
age_group_bar

#Bar plot for the count of age groups
barplot(age_group_bar, main = "Count of Age Group", xlab = "Age Group", ylab = "Count", col = c("#e3342f" ,"#f6993f", "#ffed4a","#38c172","#4dc0b5", "#3490dc", "#6574cd", "#9561e2", "#f66d9b" ), legend=TRUE)

#frequency for each age and Pro-Social Attribute Willingness
rename(count(cvbase, age,jbInsec01 ), Freq = n)
rename(count(cvbase, age,jbInsec02), Freq = n)
rename(count(cvbase, age,jbInsec03), Freq = n)
rename(count(cvbase, age,jbInsec04), Freq = n)



employment = c("employstatus_1", "employstatus_2","employstatus_3", "employstatus_4","employstatus_5","employstatus_6","employstatus_7","employstatus_8","employstatus_9","employstatus_10")
#Creating a new column, with the value that has a "1"/not "NA" for the employstatus
cvbase$employment_status <- names(cvbase[,employment])[max.col(!is.na(cvbase[,employment]) , ties.method = "first")] 
cvbase$employment_status <- as.numeric(apply(cvbase["employment_status"],1, function(x) gsub("employstatus_", "",x))) #Creating a new column specifying the employment status

#Grouping the data based on the employment status, with the count
employment_count = cvbase %>% group_by(employment_status) %>% summarise(COUNT = n())
employment_count


employment_count <- as.data.frame(employment_count, row.names = NULL, optional = FALSE)
employment_count_bar = employment_count$COUNT
names(employment_count_bar) <- employment_count$employment_status
grid.table(employment_count)
barplot(employment_count_bar, main = "Count of Employment Group", xlab = "Employment Group", ylab = "Count", col = c("#e3342f" ,"#f6993f", "#ffed4a","#38c172","#4dc0b5", "#3490dc", "#6574cd", "#9561e2", "#f66d9b","#bad80a" ))

#Dropping the columns of employstatus, since the employment status has already been stated in employment_status
cvbase = subset(cvbase, select = -c(employstatus_1, employstatus_2,employstatus_3, employstatus_4,employstatus_5,employstatus_6,employstatus_7,employstatus_8,employstatus_9,employstatus_10) )


#Q2 Focus country vs all other countries as a group
#Q2a
#Filtering the coded countries with Saudi Arabia 
saudi_arabia = cvbase %>% filter( coded_country == "Saudi Arabia")

#Dimensions of the Saudi Arabia responses
dim(saudi_arabia)
ncol(saudi_arabia) #There are 45 columns 
nrow(saudi_arabia) #There are 904 rows 


#Filtering the country group that is not Saudi Arabia
not_sa = cvbase %>% filter(coded_country != "Saudi Arabia")

#There are 39096 rows and 45 columns of data with coded_country excluding Saudi Arabia
dim(not_sa)

#Creating a datafrmae to compare the pro-social attitudes of Saudi Arabia and other countries
prosocial <- data.frame(Country = c("Saudi Arabia", "Others"), c19ProSo01 = c(mean(saudi_arabia$c19ProSo01, na.rm = TRUE), mean(not_sa$c19ProSo01, na.rm = TRUE)), 
                        c19ProSo02 = c(mean(saudi_arabia$c19ProSo02, na.rm = TRUE), mean(not_sa$c19ProSo02, na.rm = TRUE)), 
                        c19ProSo03 = c(mean(saudi_arabia$c19ProSo03, na.rm = TRUE), mean(not_sa$c19ProSo03, na.rm = TRUE)),
                        c19ProSo04 = c(mean(saudi_arabia$c19ProSo04, na.rm = TRUE), mean(not_sa$c19ProSo04, na.rm = TRUE)))
row.names(prosocial) <- prosocial$Country
prosocial <- prosocial[ , 2:ncol(prosocial)]
colnames(prosocial) <- c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
prosocial <- as.matrix(prosocial)
prosocial
barplot(main = "Mean of Corona ProSocial Behavior of Saudi Arabia and Other Countries", xlab = "Corona ProSocial Behavior", ylab = "Willingness Degree", height = prosocial, beside = TRUE, legend.text = TRUE, args.legend = list(x = "bottomright", inset = c(-0.05, -0.4))) 




#Best predictors
saudi_arabia_num <- unlist(lapply(saudi_arabia, is.numeric))  
saudi_arabia_num <- saudi_arabia[ , saudi_arabia_num] 
saudi_arabia_num = na.omit(saudi_arabia_num)
saudi_arabia_num

prosocial_sa <- data.frame(Country = c("Saudi Arabia"), c19ProSo01 = c(mean(saudi_arabia$c19ProSo01, na.rm = TRUE)), 
                        c19ProSo02 = c(mean(saudi_arabia$c19ProSo02, na.rm = TRUE)), 
                        c19ProSo03 = c(mean(saudi_arabia$c19ProSo03, na.rm = TRUE)),
                        c19ProSo04 = c(mean(saudi_arabia$c19ProSo04, na.rm = TRUE)))
row.names(prosocial_sa) <- prosocial_sa$Country
prosocial_sa <- prosocial_sa[ , 2:ncol(prosocial_sa)]
colnames(prosocial_sa) <- c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
prosocial_sa <- as.matrix(prosocial_sa)
prosocial_sa
#The mean values of all Corona ProSocial Behaviors for Saudi Arabia
barplot(main = "Mean of Corona ProSocial Behavior of Saudi Arabia", xlab = "Corona ProSocial Behavior", ylab = "Willingness Degree", height = prosocial_sa,ylim = c(-3,3))


#Correlation between Corona ProSocial Behaviour with all other values
#Q2b

pro_social_col = c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
pro_social = saudi_arabia_num[, pro_social_col]
corr_matrix <- cor(saudi_arabia_num[, !colnames(saudi_arabia_num) %in% pro_social_col], pro_social)

corr_melt = melt(corr_matrix)
head(corr_melt)
heatmap_sa <- ggplot(corr_melt, aes(Var2, Var1)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value)) +scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)
heatmap_sa

#Linear regression between c19ProSo01 and all other attributes from Saudi Arabia
lm_c19ProSo01_sa = lm(c19ProSo01 ~ ., data = saudi_arabia_num)
summary(lm_c19ProSo01_sa)


coef01_sa <- summary(lm_c19ProSo01_sa)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef01_sa[order(coef01_sa[ , 4]), ]
coef01_sa_less = coef01_sa[coef01_sa[,4] < 0.05,]
coef01_sa_less

#Linear regression between c19ProSo02 and all other attributes from Saudi Arabia
lm_c19ProSo02_sa =lm(c19ProSo02~., data = saudi_arabia_num)
summary(lm_c19ProSo02_sa)
coef02_sa <- summary(lm_c19ProSo02_sa)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef02_sa[order(coef02_sa[ , 4]), ]
coef02_sa_less = coef02_sa[coef02_sa[,4] < 0.05,]
coef02_sa_less

#Linear regression between c19ProSo03 and all other attributes from Saudi Arabia
lm_c19ProSo03_sa =lm(c19ProSo03~., data = saudi_arabia_num)
summary(lm_c19ProSo03_sa)
coef03_sa <- summary(lm_c19ProSo03_sa)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef03_sa[order(coef03_sa[ , 4]), ]

#Linear regression between c19ProSo04 and all other attributes from Saudi Arabia
lm_c19ProSo04_sa =lm(c19ProSo04~., data = saudi_arabia_num)
summary(lm_c19ProSo04_sa)
coef04_sa <- summary(lm_c19ProSo04_sa)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef04_sa[order(coef04_sa[ , 4]), ]

#Creating a dataframe to plot these R-squared values
prosocialattrnames <- c("c19ProSo01","c19ProSo02","c19ProSo03","c19ProSo04")
prosocialattrvalue_sa <- c(summary(lm_c19ProSo01_sa)$r.squared, summary(lm_c19ProSo02_sa)$r.squared,summary(lm_c19ProSo03_sa)$r.squared,summary(lm_c19ProSo04_sa)$r.squared)
prosocialvalue_df <- data.frame(prosocialattrnames,prosocialattrvalue_sa)


prosocialr2_sa <- data.frame(Country = c("Saudi Arabia"), c19ProSo01 = summary(lm_c19ProSo01_sa)$r.squared, 
                           c19ProSo02 = summary(lm_c19ProSo02_sa)$r.squared, 
                           c19ProSo03 = summary(lm_c19ProSo03_sa)$r.squared,
                           c19ProSo04 = summary(lm_c19ProSo04_sa)$r.squared)
row.names(prosocialr2_sa) <- c("Saudi Arabia")
prosocialr2_sa <- prosocialr2_sa[ , 2:ncol(prosocialr2_sa)]
colnames(prosocialr2_sa) <- c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
prosocialr2_sa <- as.matrix(prosocialr2_sa)
prosocialr2_sa

#Bar plot of the R-squared values for each pro-social attitudes for Saudi Arabia
barplot(height = prosocialr2_sa, main = "ProSocial Attitudes of Saudi Arabia and R-squared values", xlab = "ProSocial Attributes", ylab = "R-squared values", ylim = c(0,1))

#Applying a filter to filter out Saudi Arabia
not_sa_num <- unlist(lapply(not_sa, is.numeric))  

not_sa_num <- not_sa[ , not_sa_num] 
not_sa_num = na.omit(not_sa_num)
not_sa_ctry = na.omit(not_sa)


pro_social_col = c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
pro_social = not_sa_num[, pro_social_col]
corr_matrix <- cor(not_sa_num[, !colnames(saudi_arabia_num) %in% pro_social_col], pro_social)

corr_melt = melt(corr_matrix)
head(corr_melt)
heatmap_oc <- ggplot(corr_melt, aes(Var2, Var1)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value)) +scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)
heatmap_oc

#Linear regression between c19ProSo01 and all other attributes from other countries
lm_c19ProSo01_oc =lm(c19ProSo01~., data = not_sa_num)
summary(lm_c19ProSo01_oc)
#The R-squared value: 
summary(lm_c19ProSo01_oc)$r.squared
coef01_oc <- summary(lm_c19ProSo01_oc)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef01_oc[order(coef01_oc[ , 4]), ] 
coef01_oc_less = coef01_oc[coef01_oc[,4] < 0.05,]
coef01_oc[coef01_oc[,4] < 0.05,][,0]

#Linear regression between c19ProSo02 and all other attributes from other countries
lm_c19ProSo02_oc =lm(c19ProSo02~., data = not_sa_num)
summary(lm_c19ProSo02_oc)
coef02_oc <- summary(lm_c19ProSo02_oc)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef02_oc[order(coef02_oc[ , 4]), ] 
coef02_oc_less = coef02_oc[coef02_oc[,4] < 0.05,]
coef02_oc[coef02_oc[,4] < 0.05,][,0]

#Linear regression between c19ProSo03 and all other attributes from other countries
lm_c19ProSo03_oc =lm(c19ProSo03~., data = not_sa_num)
summary(lm_c19ProSo03_oc)
coef03_oc <- summary(lm_c19ProSo03_oc)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef03_oc[order(coef03_oc[ , 4]), ] 
coef03_oc_less = coef03_oc[coef03_oc[,4] < 0.05,]
coef03_oc[coef03_oc[,4] < 0.05,][,0]

#Linear regression between c19ProSo04 and all other attributes from other countries
lm_c19ProSo04_oc =lm(c19ProSo04~., data = not_sa_num)
summary(lm_c19ProSo04_oc)
coef04_oc <- summary(lm_c19ProSo04_oc)[["coefficients"]]
#Sorting based on the P-value in ascending order
coef04_oc[order(coef04_oc[ , 4]), ] 
coef04_oc_less = coef04_oc[coef04_oc[,4] < 0.05,]
coef04_oc[coef04_oc[,4] < 0.05,][,0]


prosocialr2_oc <- data.frame(Country = c("Saudi Arabia"), c19ProSo01 = c(summary(lm_c19ProSo01_sa)$r.squared, summary(lm_c19ProSo01_oc)$r.squared),
                             c19ProSo02 = c(summary(lm_c19ProSo02_sa)$r.squared, summary(lm_c19ProSo02_oc)$r.squared),
                             c19ProSo03 =c(summary(lm_c19ProSo03_sa)$r.squared,summary(lm_c19ProSo03_oc)$r.squared),
                             c19ProSo04 = c(summary(lm_c19ProSo04_sa)$r.squared,summary(lm_c19ProSo04_oc)$r.squared))
row.names(prosocialr2_oc) <- c("Saudi Arabia", "Other Countries")
prosocialr2_oc <- prosocialr2_oc[ , 2:ncol(prosocialr2_oc)]
colnames(prosocialr2_oc) <- c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
prosocialr2_oc <- as.matrix(prosocialr2_oc)
prosocialr2_oc

#A barplot for comparing the pro-social attritudes of Saudi Arabia and other countries' R-squared values
barplot(height = prosocialr2_oc, main = "ProSocial Attitudes of Saudi Arabia and other countries and R-squared values", xlab = "ProSocial Attributes", ylab = "R-squared values", ylim = c(0,1), beside =TRUE, col = c("#eb8060", "#b9e38d"))
legend("topright", 
       legend = c("Saudi Arabia", "Other Countries"), fill =c("#eb8060", "#b9e38d"))



#Q3 clustering
coviddata = read.csv("owid-covid-data.csv")

#There are 255 unique countries
unique(coviddata$location)

#Selecting new_cases, new_deaths, new_vaccinations and gdp
covid_data = coviddata %>% select(new_cases,new_deaths,new_vaccinations, gdp_per_capita,)

#covid_data but with locations added
covid_data_locations = coviddata %>% select(location,new_cases,new_deaths,new_vaccinations, gdp_per_capita,)
head(covid_data_locations)
covid_cluster_table = as.data.frame(covid_data_locations)
(head(covid_cluster_table, n =50))

#ignoring all NA values
covid_data = na.omit(covid_data)
covid_data_locations = na.omit(covid_data_locations)

summary(covid_data)

#scaling the data
covid_data_scale = scale(covid_data)

#naming each rows of data with their countries and number separated by "_"
rownames(covid_data_scale) <- paste(covid_data_locations$location, 1:dim(covid_data_locations)[1], sep = "_")

#applying kmeans cluster
set.seed(31860532)
km.out <- kmeans(covid_data_scale, centers = 7, nstart = 50)
km.clusters <- km.out$cluster

#using a dataframe to store the countries and the cluster numbers
cluster_df <- data.frame(Value = as.vector(t(covid_data_locations$location)), Cluster = km.out$cluster)
cluster_df

#finding out which cluster the Saudi Arabia country is in 
saudi_cluster =unique(subset(cluster_df, grepl("Saudi", Value))$Cluster)

#finding all the countries that are in the same cluster as Saudi Arabia
unique(subset(cluster_df, grepl(saudi_cluster, Cluster))$Value)

#Output the cluster visualisation.
fviz_cluster(list(data =covid_data_scale , cluster = km.clusters),labelsize = 10)

#The cluster countries chosen are: Australia, Canada, Germany, Japan, UK, US  
cluster_ctry = c("Australia", "Canada", "Germany", "Japan", "United Kingdom", "United States of America")
cluster_countries = not_sa_ctry[not_sa_ctry$coded_country %in% cluster_ctry,]
cluster_countries
unique(cluster_countries$coded_country)



pro_social_col = c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
pro_social = cluster_countries[, pro_social_col]
corr_matrix <- cor(cluster_countries[, !colnames(saudi_arabia_num) %in% pro_social_col], pro_social)

corr_melt = melt(corr_matrix)
head(corr_melt)
heatmap_cluster <- ggplot(corr_melt, aes(Var2, Var1)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = value)) +scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)
heatmap_cluster

#Linear regression of cluster countries with c19ProSo01 dependent variable
lm_c19ProSo01_cluster =lm(c19ProSo01~., data = cluster_countries )
summary(lm_c19ProSo01_cluster)
coef01_oc_cluster <- summary(lm_c19ProSo01_cluster)[["coefficients"]]
coef01_oc_cluster
#Sorting based on the P-value in ascending order, and finding p-values less than 0.05
ordered_cluster01 = coef01_oc_cluster[order(coef01_oc_cluster[ , 4]), ] 
ordered_cluster01_less = ordered_cluster01[ordered_cluster01[,4] < 0.05,]
ordered_cluster01_less[,0]

#Linear regression of cluster countries with c19ProSo02 dependent variable
lm_c19ProSo02_cluster =lm(c19ProSo02~., data = cluster_countries )
summary(lm_c19ProSo02_cluster)
coef02_oc_cluster <- summary(lm_c19ProSo02_cluster)[["coefficients"]]
coef02_oc_cluster
#Sorting based on the P-value in ascending order, and finding p-values less than 0.05
ordered_cluster02 = coef02_oc_cluster[order(coef02_oc_cluster[ , 4]), ] 
ordered_cluster02_less = ordered_cluster02[ordered_cluster02[,4] < 0.05,]
ordered_cluster02_less[,0]

#Linear regression of cluster countries with c19ProSo03 dependent variable
lm_c19ProSo03_cluster =lm(c19ProSo03~., data = cluster_countries )
summary(lm_c19ProSo03_cluster)
coef03_oc_cluster <- summary(lm_c19ProSo03_cluster)[["coefficients"]]
coef03_oc_cluster
#Sorting based on the P-value in ascending order, and finding p-values less than 0.05
ordered_cluster03 = coef03_oc_cluster[order(coef03_oc_cluster[ , 4]), ] 
ordered_cluster03_less = ordered_cluster03[ordered_cluster03[,4] < 0.05,]
ordered_cluster03_less 
ordered_cluster03_less[,0]

#Linear regression of cluster countries with c19ProSo04 dependent variable
lm_c19ProSo04_cluster =lm(c19ProSo04~., data = cluster_countries )
summary(lm_c19ProSo04_cluster)
coef04_oc_cluster <- summary(lm_c19ProSo04_cluster)[["coefficients"]]
coef04_oc_cluster
#Sorting based on the P-value in ascending order, and finding p-values less than 0.05
ordered_cluster04 = coef04_oc_cluster[order(coef04_oc_cluster[ , 4]), ] 
ordered_cluster04_less = ordered_cluster04[ordered_cluster04[,4] < 0.05,]
ordered_cluster04_less 
ordered_cluster04_less[,0]

#Dataframe to plot the bar plot ffor comparing r-squared values for each pro-social attitudes attributes
prosocialr2_cluster <- data.frame(Country = c("Saudi Arabia"), c19ProSo01 = c(summary(lm_c19ProSo01_sa)$r.squared, summary(lm_c19ProSo01_oc)$r.squared,summary(lm_c19ProSo01_cluster)$r.squared),
                             c19ProSo02 = c(summary(lm_c19ProSo02_sa)$r.squared, summary(lm_c19ProSo02_oc)$r.squared,summary(lm_c19ProSo02_cluster)$r.squared),
                             c19ProSo03 =c(summary(lm_c19ProSo03_sa)$r.squared,summary(lm_c19ProSo03_oc)$r.squared,summary(lm_c19ProSo03_cluster)$r.squared),
                             c19ProSo04 = c(summary(lm_c19ProSo04_sa)$r.squared,summary(lm_c19ProSo04_oc)$r.squared,summary(lm_c19ProSo04_cluster)$r.squared))
row.names(prosocialr2_cluster) <- c("Saudi Arabia", "Other Countries", "Cluster Countries")
prosocialr2_cluster <- prosocialr2_cluster[ , 2:ncol(prosocialr2_cluster)]
colnames(prosocialr2_cluster) <- c("c19ProSo01", "c19ProSo02", "c19ProSo03", "c19ProSo04")
prosocialr2_cluster <- as.matrix(prosocialr2_cluster)
prosocialr2_cluster
old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)
barplot(height = prosocialr2_cluster, main = "ProSocial Attitudes of Saudi Arabia, other countries and cluster countries and R-squared values", xlab = "ProSocial Attributes", ylab = "R-squared values", ylim = c(0,1), beside =TRUE,col = c("#eb8060", "#b9e38d", "#a1e9f0"))
legend("topright", 
       legend = c("Saudi Arabia", "Other Countries", "Cluster Countries"), fill =c("#eb8060", "#b9e38d", "#a1e9f0"))

