library(tidyverse)
library(caret)

a <- read.csv("abalone.csv")

reg_all <- lm(Rings ~  Diameter + Height, data = a)
summary(reg_all)

reg_all <- lm(Rings ~ . -Type, data = a)
summary(reg_all)

vif(lm(Rings ~ . -Type, data = a))



MALE_INFANT <- a[a$Type %in% c("M", "I"), ]
FEMALE_INFANT <- a[a$Type %in% c("F", "I"), ]


reg_all <- lm(Diameter ~ Type, data = MALE_INFANT)
summary(reg_all)

FEMALE_INFANT<- FEMALE_INFANT %>%
  mutate(Fem = ifelse(Type=="F",1,0))

reg_all <- lm(Diameter ~ Fem, data = FEMALE_INFANT)
summary(reg_all)
_________________________________________________________________________________________________________________________________

model <- read.csv("Admissions.csv")

Model4 <- glm(Admitted ~. , data = model, family = "binomial")
summary(Model4)

model <- model%>% 
  mutate(pred_prob_model4 = predict(Model4, newdata = ., type = "response")) %>%
  mutate(pred_outcome_0.75 = ifelse(pred_prob_model4 > 0.75,1,0))

Test <- model %>% mutate(accurate = 1*(Admitted == pred_outcome_0.75))
sum(Test$accurate)/nrow(Test)


pred <- prediction(model$pred_outcome_0.75,model$Admitted)


perf <- performance(pred, "tpr", "fpr") # tpr and fpr are true and false positive rates
plot(perf, colorize=T)


# calculate Area Under the Curve for this Logit Model
auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
_________________________________________________________________________________________________________________________________


if (!require(lubridate)) install.packages("lubridate") 

library(PerformanceAnalytics) 

data(managers) 
force(managers)

head(managers)
library(lubridate)

sd(managers$HAM4)*100

mean(managers$HAM4)*100



25000 * (1+Return.cumulative(managers$HAM3,geometric = TRUE))


Return.cumulative(managers$HAM3,geometric =	TRUE)
cum_ret <- Return.cumulative(managers$HAM3,geometric =	TRUE)[1]

chart.CumReturns(managers$HAM3,wealth.index =	FALSE,	geometric	=	TRUE)

25000*(1+cum_ret)

names(managers)

SharpeRatio(managers$HAM1,managers$`US 3m TR`)

SharpeRatio(managers$`SP500 TR`,managers$`US 3m TR`)

TreynorRatio(managers$HAM3,managers$`SP500 TR`,managers$`US 3m TR`)

TreynorRatio(managers$HAM3,managers$`US 3m TR`)

TreynorRatio(managers$`SP500 TR`,managers$`US 3m TR`)
_________________________________________________________________________________________________________________________________________


Exam <- read.csv("Final_Exam_Factors.csv")

library(stargazer)

factor1 <- lm(NVDA ~ MKT_RF + SMB + HML + QMJ + BAB + MOM + RF + MKT, data = Exam)
summary(factor1)

factor2 <- lm(INTC ~ MKT_RF + SMB + HML + QMJ + BAB + MOM + RF + MKT, data = Exam)
summary(factor2)


stargazer(factor1, factor2, align = TRUE, type = "html", out = "EXAMModelFull.html", report="vc*t")
_______________________________________________________________________________________________________________________________________

data <- read.csv("KAG_wrangled_dataset(1).csv",stringsAsFactors = FALSE)


data <- data %>% mutate(CTR = round(((Clicks / Impressions) * 100),4), 
                        CPC = ifelse(Clicks != 0, round(Spent / Clicks,4), Spent), 
                        CostPerConv_Total = ifelse(Total_Conversion !=0,round(Spent/Total_Conversion,4),Spent),
                        CostPerConv_Approved = ifelse(Approved_Conversion !=0,round(Spent/Approved_Conversion,4),Spent),
                        CPM = round((Spent / Impressions) * 1000, 2) )

# convert gender variable to integer
data$gender[data$gender == 'M'] <- 0
data$gender[data$gender == 'F'] <- 1
data$gender <- as.integer(data$gender)


round(nrow(filter(data, (gender == 0) & (Impressions > 10000))) / nrow(filter(data, (gender == 0)))*100,2)


data %>% filter(CPC == mean(CPC))  %>% select(xyz_campaign_id)


mean(data$CPC)

data %>% filter(xyz_campaign_id == 916) %>% select(CPC)

# write.csv(data,"CHECING_FB_MANUAL(1).csv")

data %>% filter(interest == max(interest)) %>% filter(Impressions == max(Impressions)) %>% select(ad_id)
________________________________________________________________________________________________________________________________________


arrival_rate	<- 37
service_rate_per_server	<- 65
number_of_servers	<- 13
service_rates	<- number_of_servers*service_rate_per_server
df	<- as.data.frame(number_of_servers)
df	<- cbind(df,service_rates)
df<-		df	%>%
  mutate(average_waits= (arrival_rate*60)/(service_rates	*(service_rates-arrival_rate)))

wait_plot	<- plot(x=df$number_of_servers,y=df$average_waits,type	= "b",	
                  xlab	= 'Number	of	Servers	on	Duty',	ylab	= 'Average	Customer	Wait	Times	(in	Minutes)')

______________________________________________________________________________________________________________________________________


data <- read.csv("Store_Demand_Final.csv")

data$Date <- as.Date(data$Date, format = "%m/%d/%Y")

data.xts <- xts(data[,-1], order.by = as.POSIXct(data$Date))

model <- ses(data.xts, alpha = .25, h = 5)
accuracy(model)

plot(model)

alpha <- seq(.01, .99, by = .01)
RMSE <- NA
for(i in seq_along(alpha)) {
  fit <- ses(data.xts, alpha = alpha[i], h = 5)
  RMSE[i] <- accuracy(fit)[1,2]
}

# convert to a data frame and idenitify min alpha value
alpha.fit <- data_frame(alpha, RMSE)
alpha.min <- filter(alpha.fit, RMSE == min(RMSE))
print(alpha.min)

# plot RMSE vs. alpha
ggplot(alpha.fit, aes(alpha, RMSE)) +
  geom_line() +
  geom_point(data = alpha.min, aes(alpha, RMSE), size = 2, color = "blue")  




