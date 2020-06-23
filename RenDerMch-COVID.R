################################
# Loading LIBRARIES
################################

# installing packages if necessary:
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(readxl)
library(rvest)
library(stringr)

################################
# Loading DATA
################################

# LOADING Kaggle Data Set
# original location: https://www.kaggle.com/c/covid19-global-forecasting-week-1/download/jKMTxvK1mqhnyEnsTq9H%2Fversions%2FcBgWUNa7eY5T7KL23gkv%2Ffiles%2Ftrain.csv
# moved to below location because I am not sure kaggle will be accessible in the future as it is a competition set
# downloaded from Kaggle on 25th March at 10:35 (GMT +8)
dl <- tempfile()
download.file("https://raw.githubusercontent.com/RenDerMch/COVID/master/train.csv", dl)
kaggle_train <- read_csv(dl)
colnames(kaggle_train)[3]<-"country"

# LOADING Country list of GDP PPP per Capita 
temp <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita")
temp <- temp %>% html_nodes("table")
#created data frame with Country name and GDP PPP per capita (source World Bank)
gdp_worldbank <- temp[[4]] %>% html_table %>% 
  mutate(temp=str_replace_all(`Int$`,"\\s\\(2017\\)",""),
         gdp_ppp=as.numeric(str_replace_all(temp,",","")),
         country=`Country/Territory`) %>%
  select(country,gdp_ppp)
#created data frame with Country name and GDP PPP per capita (source CIA) with countries that are not in World Bank data
gdp_cia <- temp[[5]] %>% html_table %>% 
  mutate(temp=str_replace_all(`Int$`,"\\s\\(2017\\)",""),
         gdp_ppp=as.numeric(str_replace_all(temp,",","")),
         country=`Country/Territory`) %>%
  select(country,gdp_ppp) %>%
  anti_join(gdp_worldbank,by="country")
# created a data fram with merge join of both
gdp<-bind_rows(gdp_worldbank,gdp_cia)

# LOADING Country list of Population Density 
temp <- read_html("https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population_density")
temp <- temp %>% html_nodes("table")
temp <- temp[[1]] %>% html_table(fill=TRUE) 
c<-temp[1,]
c[6]<-"density_km2"
c[2]<-"Country"
temp <- temp[-(1:4),]
colnames(temp)<-c
temp <- temp %>% select(Country,density_km2) %>% 
  mutate(country=str_replace_all(Country,"\\s\\(.+?\\)",""),
         country=str_replace_all(country,"\\[.+?\\]",""),
         density_km2=as.numeric(str_replace_all(density_km2,",",""))) %>%
  select(country,density_km2)
c<-is.na(temp$density_km2)
c<-which(c=="TRUE")
#created data frame with Country name and Popuplation Density per km^2
dens<-temp[-c,]                     

# LOADING Country list of Median Country Age
temp <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_median_age")
temp <- temp %>% html_nodes("table")
#created data frame with Country name and their Median Age in years
age <- temp[[1]] %>% html_table %>%
  mutate(country=`Country/Territory`,
         median_age=`Median(Years)`) %>%
  select(country,median_age)


# LOADING Household statistics
# original location: https://population.un.org/household/exceldata/population_division_UN_Houseshold_Size_and_Composition_2019.xlsx
# moved to below location just to be sure the data remains static
# downloaded from UN on 25th March at 12:34 (GMT +8)
dl <- tempfile()
download.file("https://github.com/RenDerMch/COVID/raw/master/population_division_UN_Houseshold_Size_and_Composition_2019(1).xlsx", dl)
householdsize <- read_xlsx(dl,sheet="UN HH Size and Composition 2019")
householdsize<-householdsize[-(1:4),c(1,3,4,41)]
colnames(householdsize)<-c("country","source","date","perc_multigen")
#created a tibble with Country name and Percentage of multi-generation households
householdsize <- householdsize %>% 
  mutate(date=as.Date(as.numeric(date),origin="1899-12-30"),
         perc_multigen=as.numeric(perc_multigen))
householdsize <- householdsize[complete.cases(householdsize),] %>%
  arrange(desc(date)) %>% group_by(country) %>% 
  summarize(perc_multigen=first(perc_multigen))

# LOADING Physicians per 1000 people by country
dl <- tempfile()
download.file("http://api.worldbank.org/v2/en/indicator/SH.MED.PHYS.ZS?downloadformat=excel", dl)
physicians <- read_xls(dl,sheet = "Data")
c<-physicians[3,]
physicians <-physicians[-(1:3),]
colnames(physicians)<-c
#created a tibble with Country name and Number of Physicians per 1000 people
physicians <- physicians %>% 
  gather(year,physicians_per_1000,`1960`:`2019`,na.rm = TRUE) %>%
  mutate(country=`Country Name`,
         physicians_per_1000=as.numeric(physicians_per_1000)) %>%
  select(country,year,physicians_per_1000) %>%
  arrange(desc(year)) %>% group_by(country) %>%
  summarize(physicians_per_1000=first(physicians_per_1000))

# LOADING Hospital beds per 1000 people by country
dl <- tempfile()
download.file("http://api.worldbank.org/v2/en/indicator/SH.MED.BEDS.ZS?downloadformat=excel", dl)
beds <- read_xls(dl,sheet = "Data")
c<-beds[3,]
beds <-beds[-(1:3),]
colnames(beds)<-c
#created a tibble with Country name and Number of beds per 1000 people
beds <- beds %>% 
  gather(year,beds_per_1000,`1960`:`2019`,na.rm = TRUE) %>%
  mutate(country=`Country Name`,
         beds_per_1000=as.numeric(beds_per_1000)) %>%
  select(country,year,beds_per_1000) %>%
  arrange(desc(year))  %>% group_by(country) %>%
  summarize(beds_per_1000=first(beds_per_1000))

################################
# Joining DATA
################################

# cleaning up GDP table for better fit for joining by replacing Country names to match countries is Kaggle source data
gdp[,1]<- gdp$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Republic of the","Republic of the Congo") %>%
  str_replace_all("Congo, Democratic Republic of the","Congo (Kinshasa)") %>%
  str_replace_all("Côte d'Ivoire","Cote d'Ivoire") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("United States","US") %>%
  str_replace_all("The Bahamas","Bahamas, The") %>%
  str_replace_all("The Gambia","Gambia, The")

# joining GDP to Kaggle_train set and creating data_set
data_set <- kaggle_train %>%
  left_join(gdp,by="country")

# cleaning up Density table for better fit for joining by replacing Country names to match countries is Kaggle source data
c<-dens$country[148]
dens[,1]<- dens$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Republic of the Congo","Republic of the Congo") %>%
  str_replace_all("Democratic Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Ivory Coast","Cote d'Ivoire") %>%
  str_replace_all("Gambia","Gambia, The") %>%
  str_replace_all("Vatican City","Holy See") %>%
  str_replace_all("South Korea","Korea, South") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("Bahamas","The Bahamas") %>%
  str_replace_all(c,"Ukraine") %>%
  str_replace_all("United States","US")

# joining Population Density to the data_set
data_set <- data_set %>%
  left_join(dens,by="country")

# cleaning up AGE table for better fit for joining by replacing Country names to match countries is Kaggle source data
age[,1]<- age$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Republic of the Congo","Republic of the Congo") %>%
  str_replace_all("Democratic Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Ivory Coast","Cote d'Ivoire") %>%
  str_replace_all("Gambia","Gambia, The") %>%
  str_replace_all("Vatican City","Holy See") %>%
  str_replace_all("South Korea","Korea, South") %>%
  str_replace_all("Bahamas","The Bahamas") %>%
  str_replace_all("United States","US") %>%
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Republic of the","Republic of the Congo") %>%
  str_replace_all("Congo, Democratic Republic of the","Congo (Kinshasa)") %>%
  str_replace_all("Côte d'Ivoire","Cote d'Ivoire") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("United States","US") %>%
  str_replace_all("Bahamas, The","The Bahamas") %>%
  str_replace_all("The Gambia","Gambia, The")%>%
  str_replace_all("Eswatini (Swaziland)","Eswatini")


# joining Population AGE to the data_set
data_set <- data_set %>%
  left_join(age,by="country")

# cleaning up House holde multigen table for better fit for joining by replacing Country names to match countries is Kaggle source data
householdsize[,1]<- householdsize$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Republic of the Congo","Republic of the Congo") %>%
  str_replace_all("Democratic Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Ivory Coast","Cote d'Ivoire") %>%
  str_replace_all("Gambia","Gambia, The") %>%
  str_replace_all("Vatican City","Holy See") %>%
  str_replace_all("South Korea","Korea, South") %>%
  str_replace_all("Bahamas","The Bahamas") %>%
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Republic of the","Republic of the Congo") %>%
  str_replace_all("Dem. Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Côte d'Ivoire","Cote d'Ivoire") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("United States of America","US") %>%
  str_replace_all("Bahamas, The","The Bahamas") %>%
  str_replace_all("The Gambia","Gambia, The")%>%
  str_replace_all("Eswatini (Swaziland)","Eswatini") %>%
  str_replace_all("Congo","Republic of the Congo") %>%
  str_replace_all("Iran (Islamic Republic of)","Iran") %>%
  str_replace_all("United States of America","US")%>%
  str_replace_all("United Republic of Tanzania","Tanzania")%>%
  str_replace_all("Venezuela (Bolivarian Republic of)","Venezuela")%>%
  str_replace_all("Viet Nam","Vietnam")

# joining Population household multigen to the data_set
data_set <- data_set %>%
  left_join(householdsize,by="country")

# cleaning up Physicians per 1000 people table for better fit for joining by replacing Country names to match countries is Kaggle source data
physicians[,1]<- physicians$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Rep.","Republic of the Congo") %>%
  str_replace_all("Congo, Dem. Rep.","Congo (Kinshasa)") %>%
  str_replace_all("Ivory Coast","Cote d'Ivoire") %>%
  str_replace_all("Vatican City","Holy See") %>%
  str_replace_all("South Korea","Korea, South") %>%
  str_replace_all("Bahamas, The","The Bahamas") %>%
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Republic of the","Republic of the Congo") %>%
  str_replace_all("Dem. Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Côte d'Ivoire","Cote d'Ivoire") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("United States of America","US") %>%
  str_replace_all("Gambia, The","The Gambia")%>%
  str_replace_all("Eswatini (Swaziland)","Eswatini") %>%
  str_replace_all("Congo","Republic of the Congo") %>%
  str_replace_all("Iran, Islamic Rep.","Iran") %>%
  str_replace_all("United States of America","US")%>%
  str_replace_all("United Republic of Tanzania","Tanzania")%>%
  str_replace_all("Venezuela (Bolivarian Republic of)","Venezuela")%>%
  str_replace_all("Viet Nam","Vietnam")%>%
  str_replace_all("Brunei Darussalam","Brunei")%>%
  str_replace_all("Egypt, Arab Rep.","Egypt")%>%
  str_replace_all("Korea, Rep.","Korea, South")%>%
  str_replace_all("Kyrgyz Republic","Kyrgyzstan")%>%
  str_replace_all("Russian Federation","Russia")%>%
  str_replace_all("St. Vincent and the Grenadines","Saint Vincent and the Grenadines")%>%
  str_replace_all("Slovak Republic","Slovakia")%>%
  str_replace_all("United States","US")%>%
  str_replace_all("Venezuela, RB","Venezuela")

# joining Population Physicians to the data_set
data_set <- data_set %>%
  left_join(physicians,by="country")

# cleaning up Beds per 1000 people table for better fit for joining by replacing Country names to match countries is Kaggle source data
beds[,1]<- beds$country %>% 
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Rep.","Republic of the Congo") %>%
  str_replace_all("Congo, Dem. Rep.","Congo (Kinshasa)") %>%
  str_replace_all("Ivory Coast","Cote d'Ivoire") %>%
  str_replace_all("Vatican City","Holy See") %>%
  str_replace_all("South Korea","Korea, South") %>%
  str_replace_all("Bahamas, The","The Bahamas") %>%
  str_replace_all("Czech Republic","Czechia") %>%
  str_replace_all("Congo, Republic of the","Republic of the Congo") %>%
  str_replace_all("Dem. Republic of the Congo","Congo (Kinshasa)") %>%
  str_replace_all("Côte d'Ivoire","Cote d'Ivoire") %>%
  str_replace_all("Taiwan","Taiwan*") %>%
  str_replace_all("United States of America","US") %>%
  str_replace_all("Gambia, The","The Gambia")%>%
  str_replace_all("Eswatini (Swaziland)","Eswatini") %>%
  str_replace_all("Congo","Republic of the Congo") %>%
  str_replace_all("Iran, Islamic Rep.","Iran") %>%
  str_replace_all("United States of America","US")%>%
  str_replace_all("United Republic of Tanzania","Tanzania")%>%
  str_replace_all("Venezuela (Bolivarian Republic of)","Venezuela")%>%
  str_replace_all("Viet Nam","Vietnam")%>%
  str_replace_all("Brunei Darussalam","Brunei")%>%
  str_replace_all("Egypt, Arab Rep.","Egypt")%>%
  str_replace_all("Korea, Rep.","Korea, South")%>%
  str_replace_all("Kyrgyz Republic","Kyrgyzstan")%>%
  str_replace_all("Russian Federation","Russia")%>%
  str_replace_all("St. Vincent and the Grenadines","Saint Vincent and the Grenadines")%>%
  str_replace_all("Slovak Republic","Slovakia")%>%
  str_replace_all("United States","US")%>%
  str_replace_all("Venezuela, RB","Venezuela")

# joining Population Beds to the data_set
data_set <- data_set %>%
  left_join(beds,by="country")


################################
# Cleaning data set
################################

#dealing with NAs
mean_gdp<-mean(data_set$gdp_ppp, na.rm = TRUE) # findging the average for GDP
mean_dens<-mean(data_set$density_km2, na.rm = TRUE) # findging the average for density
mean_age<-mean(data_set$median_age, na.rm = TRUE) # findging the average for mean age
mean_multi<-mean(data_set$perc_multigen, na.rm = TRUE) # findging the average for multigeneration households
mean_phy<-mean(data_set$physicians_per_1000, na.rm = TRUE) # findging the average for number of physicians
mean_beds<-mean(data_set$beds_per_1000, na.rm = TRUE) # findging the average for GDP for number of beds

data_set<- data_set %>%  #replacing NAs with average for each of the statistical measures
  mutate(gdp_ppp=ifelse(is.na(gdp_ppp),mean_gdp,gdp_ppp),
         density_km2=ifelse(is.na(density_km2),mean_dens,density_km2),
         median_age=ifelse(is.na(median_age),mean_age,median_age),
         perc_multigen=ifelse(is.na(perc_multigen),mean_multi,perc_multigen),
         physicians_per_1000=ifelse(is.na(physicians_per_1000),mean_phy,physicians_per_1000),
         beds_per_1000=ifelse(is.na(beds_per_1000),mean_beds,beds_per_1000))

################################
# Spliting DATA set into train and test
################################

# adding region as a combination of province and country as there are for example 2 Georgias (once a country and once a state)
data_set3 <- data_set %>% mutate(region=ifelse(is.na(`Province/State`),country,paste(country,"/",`Province/State`)),
                                 prev_day=0) 

# removing China as we are not focusing on China
data_set2 <- data_set3 %>% filter(!country=="China") 

# adding previous day into the data set
for (i in 2:nrow(data_set2)) {
     data_set2$prev_day[i] <- data_set2$ConfirmedCases[i-1]
}

#rearrenging columns
data_set2 <- data_set2[,c(1,15,2,3,6,7,16,8:14,4,5)]

# setting prev_day=0 in for each change of state
data_set2 <- data_set2 %>% 
  mutate(growth=
             ifelse(ConfirmedCases/prev_day<1|prev_day==0,
                    1,ConfirmedCases/prev_day))

# rearrenging columns
data_set2 <- data_set2[,c(1:7,17,8:16)]

# splitting the whole TRAINING SET into Train and Test based on date
train_index<- which(data_set2$Date<="2020-03-11")
train_set<-data_set2[train_index,]
test_set<-data_set2[-train_index,]

################################
# Loss function
################################

RMSLE <- function(true, predicted){
  sqrt(mean((log(predicted+1) - log(true+1))^2))
}

RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}

################################
# Logistic regression - based on date
################################

train_glm <- train(growth ~ Date,                   #training for GLM given parameters
                   method="glm", data=train_set)

prediction_glm <- predict(train_glm,test_set)     #predicitng based on test_set (not overtraining as it is not the final set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- data_frame(method = "GLM - date", RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction)
                      , RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                      REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis)  #creating table with results

results %>% knitr::kable()                   # printing result table with column alignment


################################
# Logistic regression - based on date + GDP
################################

train_glm <- train(growth ~ Date + gdp_ppp,                    #training for GLM given parameters
                   method="glm", data=train_set)

prediction_glm <- predict(train_glm,test_set)     #predicitng based on test_set (not overtraining as it is not the final set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                          data_frame(method = "GLM - date + GDP", 
                                     RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                     RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                     REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment






################################
# Logistic regression - based on date + GDP + density
################################

train_glm <- train(growth ~ Date + gdp_ppp + density_km2,                    #training for GLM given parameters
                   method="glm", data=train_set)

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + density", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment


################################
# Logistic regression - based on date + GDP + median age
################################

train_glm <- train(growth ~ Date + gdp_ppp + median_age, 
                   method="glm", data=train_set)                   #training for GLM given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + median age", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment

write.table(test_set,"TEST00.csv")

################################
# Logistic regression - based on date + GDP + median age + multigen household
################################

train_glm <- train(growth ~ Date + gdp_ppp + median_age + perc_multigen, 
                   method="glm", data=train_set)                   #training for GLM given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + median age + multigen household", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment



################################
# Logistic regression - based on date + GDP + median age + physicians
################################

train_glm <- train(growth ~ Date + gdp_ppp + median_age + physicians_per_1000, 
                   method="glm", data=train_set)                   #training for GLM given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + median age + physicians", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment


################################
# Logistic regression - based on date + GDP + median age  + beds
################################

train_glm <- train(growth ~ Date + gdp_ppp + median_age + beds_per_1000, 
                   method="glm", data=train_set)                   #training for GLM given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + median age + beds", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment



################################
# Logistic regression - based on date + GDP + density + median age + multingen household + physicians + beds
################################

train_glm <- train(growth ~ Date + gdp_ppp + density_km2 + median_age + perc_multigen
                                      + physicians_per_1000 + beds_per_1000, 
                                  method="glm", data=train_set)                   #training for GLM given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_glm <- predict(train_glm,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_glm
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "GLM - date + GDP + density + median age + multigen + physicians + beds", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment



################################
# N nearest neighbors - date + GDP + median age
################################

train_test <- train(growth ~ Date + gdp_ppp + median_age, 
                   method="knn", data=train_set)                   #training for KNN given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_test <- predict(train_test,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_test
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "KNN - date + GDP + median age", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment

################################
# Projection Pursuit Regression - date + GDP + median age
################################

train_test <- train(growth ~ Date + gdp_ppp + median_age, 
                    method="ppr", data=train_set)                   #training for PPR given parameters

#predicitng based on test_set (not overtraining as it is not the final set)
prediction_test <- predict(train_test,test_set)

#setting prediction equal to 0 for first day of the data set as we are predicting growth and that requires prior data
vec1<-which(test_set$Date=="2020-03-12")
vec2<-which(!test_set$Date=="2020-03-12")
predictions_gr <- prediction_test
test_set<-test_set %>% mutate(prediction=0)
for (i in vec1) {
  test_set$prediction[i] <- (test_set$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {              #calculating prediction as previous day * predicted growth
  test_set$prediction[i] <- (test_set$prediction[i-1]*predictions_gr[i])
}

temp1 <- test_set %>% filter(Date=="2020-03-24") %>% 
  summarize(conf=sum(ConfirmedCases),pred=sum(prediction),dis=(conf-pred))

results <- bind_rows(results,                      # adding line to the result table
                     data_frame(method = "PPR - date + GDP + median age", 
                                RMSLE = RMSLE(test_set$ConfirmedCases,test_set$prediction), 
                                RMSE = RMSE(test_set$ConfirmedCases,test_set$prediction),
                                REAL= temp1$conf, PRED= temp1$pred, DISCREPANCY = temp1$dis))

results %>% knitr::kable()                   # printing result table with column alignment

################################
# Loading FINAL TEST dataset
################################


# LOADING Kaggle Data Set
# original location: https://www.kaggle.com/c/covid19-global-forecasting-week-1/download/jKMTxvK1mqhnyEnsTq9H%2Fversions%2FcBgWUNa7eY5T7KL23gkv%2Ffiles%2Ftrain.csv
# moved to below location because I am not sure kaggle will be accessible in the future as it is a competition set
# downloaded from Kaggle on 25th March at 10:35 (GMT +8)
dl <- tempfile()
download.file("https://raw.githubusercontent.com/RenDerMch/COVID/master/test.csv", dl)
kaggle_test <- read_csv(dl)
colnames(kaggle_test)[3]<-"country"
remove_index<- which(kaggle_test$Date<="2020-03-24") #removing date that are already contained in the traing/test sets
kaggle_test<-kaggle_test[-remove_index,]
# creating region as combination of state and country and removing China - same as training set
kaggle_test <- kaggle_test %>% mutate(region=ifelse(is.na(`Province/State`),country,paste(country,"/",`Province/State`)))
kaggle_test <- kaggle_test %>% filter(!country=="China") 

# joining GDP to Kaggle_train set and creating data_set
kaggle_test <- kaggle_test %>%
  left_join(gdp,by="country")

# joining Population AGE to the data_set
kaggle_test <- kaggle_test %>%
  left_join(age,by="country")

# adding a the last day of TRAIN set
last_day<- data_set2 %>% filter(Date=="2020-03-24") %>% mutate(prev_day=ConfirmedCases) %>% select(region,prev_day)

kaggle_test <- kaggle_test %>%
  left_join(last_day,by="region")

#dealing with NAs
mean_gdp<-mean(kaggle_test$gdp_ppp, na.rm = TRUE)
mean_age<-mean(kaggle_test$median_age, na.rm = TRUE)

kaggle_test<- kaggle_test %>% 
  mutate(gdp_ppp=ifelse(is.na(gdp_ppp),mean_gdp,gdp_ppp),
         median_age=ifelse(is.na(median_age),mean_age,median_age))

################################
# Final train on full set and prediction
################################


train_glm <- train(growth ~ Date + gdp_ppp + median_age,      #training as per the chosen model
                   method="glm", data=data_set2)

prediction_glm <- predict(train_glm,kaggle_test)    #predicting growth

vec1<-which(kaggle_test$Date=="2020-03-25")
vec2<-which(!kaggle_test$Date=="2020-03-25")
predictions_gr <- prediction_glm
kaggle_test<-kaggle_test %>% mutate(prediction=0)
for (i in vec1) {
  kaggle_test$prediction[i] <- (kaggle_test$prev_day[i]*predictions_gr[i])
}
for (i in vec2) {
  kaggle_test$prediction[i] <- (kaggle_test$prediction[i-1]*predictions_gr[i])
}

kaggle_result<- kaggle_test %>% select (ForecastId,`Province/State`,country,Lat,Long,Date,prediction)

head(kaggle_result)

write.table(kaggle_result,"KaggleResult.csv")

kaggle_result %>% filter(Date=="2020-04-23") %>% summarize(pred=sum(prediction))

