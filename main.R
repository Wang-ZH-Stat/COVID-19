library(tidyverse)
library(openxlsx)
library(VIM)
library(mice)
library(MatchIt)
library(ggTimeSeries)
library(RColorBrewer)
library(ggradar)
library(scales)
library(gridExtra)
library(cpm)
library(plm)
library(lmtest)
library(glmnet)
library(quantreg) 
library(pastecs)
library(DataExplorer)

# data cleaning
owid_data <- read.csv('owid-covid-data.csv', header=T, na.strings=c("NA"))
owid_data <- data.frame(owid_data)
owid_data[which(owid_data$location == 'Czechia'), 3] <- 'Czech Republic'
owid_data[which(owid_data$location == 'Slovakia'), 3] <- 'Slovak Republic'
owid_data <- owid_data[which(owid_data$continent == 'Europe'), ]

country_list <- names(table(owid_data$location))
country_list <- country_list[c(c(1:16), 18, c(20:22), 24, c(26:33), 35, c(37:50))]
owid_data <- owid_data[which(owid_data$location %in% country_list), ]

policy_data <- read.csv('OxCGRT_latest.csv', header=T, na.strings=c("NA"))
policy_data <- data.frame(policy_data)
policy_data <- policy_data[which(policy_data$CountryName %in% country_list), ]
policy_data <- policy_data[which(policy_data$Jurisdiction == 'NAT_TOTAL'), ]
table(policy_data$CountryName)
table(owid_data$location)

# Policy data cleaning
names(policy_data)[7:39] <- c('C1', 'C1.', 'C2', 'C2.', 'C3', 'C3.', 'C4', 'C4.', 
                              'C5', 'C5.', 'C6', 'C6.', 'C7', 'C7.', 'C8', 
                              'E1', 'E1.', 'E2', 'E3', 'E4', 'H1', 'H1.', 'H2', 
                              'H3', 'H4', 'H5', 'H6', 'H6.', 'H7', 'H7.', 'H8', 
                              'H8.', 'M1')
names(policy_data)[42:51] <- c('SI.', 'SI', 'SLI.', 'SLI', 'GRI.', 'GRI', 'CHI.', 'CHI', 'ESI.', 'ESI')
po_col_list <- c('CountryName', 'CountryCode', 'Date', 'C1', 'C2', 'C3', 'C4', 'C5', 
                 'C6', 'C7', 'C8', 'E1', 'E2', 'H1', 'H2', 'H3', 
                 'H6', 'H7', 'H8', 'SI', 'GRI', 'CHI', 'ESI')
policy_data <- policy_data[, po_col_list]
policy_data$Date <- as.Date(as.character(policy_data$Date), "%Y%m%d")
names(policy_data)[1:3] <- c('country', 'code', 'date')

# Country data cleaning
owid_data <- owid_data[, c(3, 1, 4, 11:21, 46, 49:63)]
names(owid_data)[1:3] <- c('country', 'code', 'date')
owid_data$date <- as.Date(as.character(owid_data$date), "%Y-%m-%d")

country_data <- data.frame(owid_data %>%
  group_by(country) %>%
  summarise(population = mean(population), population_density = mean(population_density), 
            median_age = mean(median_age), aged_65_older = mean(aged_65_older), 
            aged_70_older = mean(aged_70_older), gdp_per_capita = mean(gdp_per_capita), 
            extreme_poverty = mean(extreme_poverty), cardiovasc_death_rate = mean(cardiovasc_death_rate), 
            diabetes_prevalence = mean(diabetes_prevalence), female_smokers = mean(female_smokers), 
            male_smokers = mean(male_smokers), hospital_beds_per_thousand = mean(hospital_beds_per_thousand), 
            life_expectancy = mean(life_expectancy), human_development_index = mean(human_development_index)))

# Liechtenstein, Faeroe Islands, Andorra, Kosovo, Monaco, San Marino
country_data <- country_data[-c(2, 13, 22, 24, 29, 36), ]
country_data <- country_data[, -8]
row.names(country_data) <- c(1:38)
country_data$aged_70_older[31] <- mean(country_data[-31, ]$aged_70_older)/mean(country_data[-31, ]$aged_65_older)*country_data$aged_65_older[31]

# write.xlsx(country_data, "country data.xlsx")
summary(country_data[c(2:14)])

# Combine
owid_data <- owid_data[, c(1:15)]
country_list <- country_data$country
owid_data <- owid_data[which(owid_data$country %in% country_list), ]
policy_data <- policy_data[which(policy_data$country %in% country_list), ]
for(i in c(1:38)){
  temp <- owid_data[which(owid_data$country == country_list[i]), ]
  print(temp[1,c(1,3)])
}

temp <- owid_data[which(owid_data$date > '2020-03-14'),]
mean(is.na(temp$total_cases_per_million))
mean(is.na(temp$new_cases_smoothed_per_million))

aggr(owid_data, prop=FALSE, numbers=TRUE)
matrixplot(owid_data)

owid_data_my <- owid_data[which(owid_data$date > '2020-03-14'), ]
owid_data_my <- owid_data_my[which(owid_data_my$date < '2021-12-01'), ]
# 626 days
policy_data_my <- policy_data[which(policy_data$date > '2020-03-14'), ]
policy_data_my <- policy_data_my[which(policy_data_my$date < '2021-12-01'), ]

unique(policy_data_my$country)
unique(owid_data_my$country)
policy_data_my <- policy_data_my[sort(policy_data_my$country, index.return=TRUE)$ix, ]

# join the data
mydata <- data.frame(policy_data_my, owid_data_my[, c(4:15)])
row.names(mydata) <- c(1:nrow(mydata))

# missing values
aggr(mydata[, c(4:23)], bars = FALSE)
aggr(mydata[, c(24:35)], bars = FALSE)
md.pattern(mydata[, c(4:27)])
mydata[which(is.na(mydata$ESI) == 'TRUE'), ]

summary(mydata)
stat.desc(mydata)[1,]
stat.desc(mydata)[13,]



# impute data
data <- mydata
for(i in c(4:23)){
  for(j in c(1:nrow(mydata))){
    if(is.na(mydata[j,i]) == 'TRUE'){
      data[j,i] <- data[j-1,i]
    }
  }
}
aggr(data[, c(4:23)], bars = FALSE)

for(j in c(1:nrow(mydata))){
  if(is.na(mydata[j,30]) == 'TRUE'){
    data[j,30] <- mean(data[which(is.na(mydata[,30]) == 'FALSE' & mydata$date == mydata[j,3]), 30])
  }
}
aggr(data[, c(24:35)], bars = FALSE)

# write.csv(data, "data.csv", row.names = FALSE)

summary(data[c(4:23)])
plot_histogram(data[,c(4:23)])
create_report(data)
plot_bar(data)
p11 <- plot_correlation(data[,c(4:19)], type = "continuous", 
                        ggtheme = theme_bw())



date_data <- data %>% 
  group_by(date) %>%
  summarise(C1 = mean(C1), C2 = mean(C2), C3=mean(C3), C4 = mean(C4), 
            C5 = mean(C5), C6 = mean(C6), C7=mean(C7), C8 = mean(C8),
            E1 = mean(E1), E2 = mean(E2), H1=mean(H1), H2 = mean(H2), 
            H3 = mean(H3), H6 = mean(H6), H7=mean(H7), H8 = mean(H8),
            SI = mean(SI), GRI = mean(GRI), CHI = mean(CHI), ESI = mean(ESI), 
            NCS = mean(new_cases_smoothed_per_million),
            NDS = mean(new_deaths_smoothed_per_million),
            rate = mean(reproduction_rate))
date_data <- data.frame(date_data)
matrixplot(date_data)

# write.xlsx(date_data, "date data.xlsx")

policy <- rep(names(date_data)[2:17], nrow(date_data))
strength <- c()
date <- c()
for(i in c(1:nrow(date_data))){
  strength <- c(strength, t(date_data[i, c(2:17)]))
  date <- as.Date(c(date, rep(date_data[i,1], 16)))
}
strength_data <- data.frame(date, strength, policy)
p12 <- ggplot(data=strength_data, aes(x=date, y = strength, color=policy)) + geom_line(size=1.2) +
  facet_wrap(~policy, ncol=2, scales = 'free', strip.position = 'left') + theme_bw() + 
  theme(legend.position = "none")+ labs(y = 'policy strength', x = 'month') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14))

grid.arrange(p11, p12, ncol=2, widths = c(0.4,0.6))



############################
# top countries
############################
temp_top <- country_data[,c(1,2)]
temp_top <- temp_top[sort(temp_top$population, index.return=TRUE)$ix, ]
top_country <- temp_top$country[c(38:29)]
top_data <- data[which(data$country %in% top_country),]

ggplot_calendar_heatmap(date_data,'date','NCS') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = '#33CC00') +
  facet_wrap(~Year, ncol = 1) + theme_bw() + 
  theme(axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15))

ggplot_calendar_heatmap(date_data,'date','NDS') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = '#FF6633') +
  facet_wrap(~Year, ncol = 1) + theme_bw() +  
  theme(axis.title.x=element_text(size=15), 
        axis.title.y=element_text(size=15))

set_col <- 'Accent'
# display.brewer.pal(4, set_col)
# brewer.pal(4, "Set3")


p1 <- ggplot_calendar_heatmap(date_data,'date','GRI') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = brewer.pal(4, set_col)[1]) +
  facet_wrap(~Year, ncol = 1) + theme_bw() +  
  theme(axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13))

p2 <- ggplot_calendar_heatmap(date_data,'date','SI') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = brewer.pal(4, set_col)[2]) +
  facet_wrap(~Year, ncol = 1) + theme_bw() +  
  theme(axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13))

p3 <- ggplot_calendar_heatmap(date_data,'date','CHI') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = brewer.pal(4, set_col)[3]) +
  facet_wrap(~Year, ncol = 1) + theme_bw() +  
  theme(axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13))

p4 <- ggplot_calendar_heatmap(date_data,'date','ESI') + 
  ylab(NULL) +
  scale_fill_continuous(low = 'white', high = brewer.pal(4, set_col)[4]) +
  facet_wrap(~Year, ncol = 1) + theme_bw() +  
  theme(axis.title.x=element_text(size=13), 
        axis.title.y=element_text(size=13))

grid.arrange(p1, p2, p3, p4, ncol=2)

# change point
shapiro.test(date_data$rate)
cpm.res <- processStream(date_data$rate, cpmType = "Mann-Whitney",
                             startup = 100, ARL0 = 7000)

cpm.res$detectionTimes
plot(date_data$rate, type = "l", col = "steelblue", lwd = 2)
abline(v = cpm.res$detectionTimes, lwd = 3.5, col = "red")


# 100,130,184,335,485,569
dete_list <- c(100,130,184,335,485,569)

g1 <- ggplot_waterfall(date_data,'date','rate') + 
  theme_bw()+ theme(legend.position = "none")+ labs(y = 'reproduction rate') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14))+ 
  scale_x_date(date_breaks="2 month",date_labels="%Y-%m") + 
  labs(x='month') + 
  geom_vline(xintercept = as.Date('2020-03-14') + dete_list, lwd=1.5, col = '#CC9999') +
  annotate("text", x = as.Date('2020-03-14') + dete_list[1] - 30, y = 2, label = "2020-06-22") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[1] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[1], 
           ymin = 1.94, ymax = 2.06, alpha = .2) +
  annotate("text", x = as.Date('2020-03-14') + dete_list[2] - 30, y = 1.75, label = "2020-07-22") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[2] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[2], 
           ymin = 1.69, ymax = 1.81, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[3] - 30, y = 2, label = "2020-09-14") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[3] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[3], 
           ymin = 1.94, ymax = 2.06, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[4] - 30, y = 2, label = "2021-02-12") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[4] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[4], 
           ymin = 1.94, ymax = 2.06, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[5] - 30, y = 2, label = "2021-06-12") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[5] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[5], 
           ymin = 1.94, ymax = 2.06, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[6] - 30, y = 2, label = "2021-10-04") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[6] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[6], 
           ymin = 1.94, ymax = 2.06, alpha = .2)


g2 <- ggplot_waterfall(date_data,'date','NCS') + 
  theme_bw()+ theme(legend.position = "none")+ labs(y = 'new cases smoothed per million') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14))+ 
  scale_x_date(date_breaks="2 month",date_labels="%Y-%m") + 
  labs(x='month') + 
  geom_vline(xintercept = as.Date('2020-03-14') + dete_list, lwd=1.5, col = '#CC9999') +
  annotate("text", x = as.Date('2020-03-14') + dete_list[1] - 30, y = 500, label = "2020-06-22") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[1] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[1], 
           ymin = 480, ymax = 520, alpha = .2) +
  annotate("text", x = as.Date('2020-03-14') + dete_list[2] - 30, y = 400, label = "2020-07-22") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[2] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[2], 
           ymin = 380, ymax = 420, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[3] - 30, y = 500, label = "2020-09-14") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[3] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[3], 
           ymin = 480, ymax = 520, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[4] - 30, y = 500, label = "2021-02-12") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[4] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[4], 
           ymin = 480, ymax = 520, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[5] - 30, y = 500, label = "2021-06-12") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[5] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[5], 
           ymin = 480, ymax = 520, alpha = .2)+
  annotate("text", x = as.Date('2020-03-14') + dete_list[6] - 30, y = 500, label = "2021-10-04") + 
  annotate("rect", xmin = as.Date('2020-03-14') + dete_list[6] - 60, 
           xmax = as.Date('2020-03-14') + dete_list[6], 
           ymin = 480, ymax = 520, alpha = .2)

grid.arrange(g1, g2, ncol=1)

# deaths
ggplot_waterfall(date_data,'date','NDS') + 
  theme_bw()+ theme(legend.position = "none") + labs(y = 'new deaths smoothed per million') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14))+ 
  scale_x_date(date_breaks="2 month",date_labels="%Y-%m") + 
  labs(x='month')


# top country
ggplot(top_data, aes(x = date, y = new_cases_smoothed_per_million, group = country, fill = country)) +
  stat_steamgraph(alpha = 0.8) + scale_fill_brewer(palette = "RdYlBu") + 
  theme_bw() + labs(y = 'new cases smoothed per million') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14)) + 
  scale_x_date(date_breaks="2 month",date_labels="%Y-%m") + 
  labs(x='month')


# varietas
variant_data <- read.csv('varietas.csv', header=T, na.strings=c("NA"))
variant_data$date <- as.Date(variant_data$date)

variant_data <- variant_data[which(variant_data$location %in% top_country), ]
table(variant_data$location)

variant_data <- variant_data[which(variant_data$date > '2020-07-31'), ]
variant_data <- variant_data[which(variant_data$date < '2021-11-01'), ]
# length(unique(varietas_data[which(varietas_data$location == 'Poland'), 2]))
# length(unique(varietas_data[which(varietas_data$location == 'France'), 2]))

variant_data <- variant_data[which(variant_data$location != 'Poland'), ]
variant_data <- variant_data[which(variant_data$location != 'Ukraine'), ]
variant_data <- variant_data[which(variant_data$location != 'Netherlands'), ]
variant_data <- variant_data[which(variant_data$location != 'Russia'), ]

variant_data[which(variant_data$variant == 'non_who'), ]

data.frame(variant_data[which(variant_data$location == 'United Kingdom'), c(3:5)] %>%
  group_by(variant) %>%
  summarise_all(list(mean = mean, median = median)))

# France: Alpha, Delta, B.1.160, Beta
# Germany: Delta, Alpha, B.1.177, B.1.160
# Italy: Delta, Alpha, B.1.177, B.1.160, Gamma, B.1.258
# Russia: Delta, Alpha
# Spain: B.1.177, Alpha, Delta, Beta, Gamma
# United Kingdom: Delta, Alpha, B.1.177, B.1.258

variant_list <- c('Alpha', 'B.1.160', 'B.1.177', 'B.1.258','Beta', 'Delta')

variant_data <- variant_data[which(variant_data$variant %in% variant_list), ]
table(variant_data$location)


variant_mean <- variant_data[,c(3:5)] %>%
  group_by(variant) %>%
  summarise_all(list(mean = mean))
variant_mean <- data.frame(variant_mean)
variant_mean <- variant_mean[sort(variant_mean$perc_sequences_mean, index.return=TRUE)$ix, ]

names(variant_data)[1] <- 'country'
names(variant_data)[5] <- 'percentage'
row.names(variant_data) <- c(1:nrow(variant_data))

ggplot(data = variant_data, aes(x=date, y=percentage, 
                                group = variant, 
                                color = variant,
                                shape = variant)) +
  geom_line(size = 1.2) + geom_point(size=3) + 
  facet_wrap(~country, nrow=2) +
  scale_x_date(date_breaks="3 month",date_labels="%Y-%m") + 
  labs(x='month') + 
  theme_bw()+ 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        legend.position = "top") +
  scale_color_brewer(palette = "Paired")

ggplot(data = variant_data, mapping = aes(x=date, y=percentage, 
                                group = variant, 
                                fill = variant)) +
  geom_area(position = 'fill', alpha = 0.8)+ 
  facet_wrap(~country, nrow=2) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_x_date(date_breaks="3 month",date_labels="%Y-%m") + 
  labs(x='month') + 
  theme_bw()+ 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        legend.position = "top") +
  scale_fill_brewer(palette = "Pastel2")


# reproduction
ggplot(data = top_data, aes(x=date, y=reproduction_rate, 
                                group = country, 
                                color = country)) +
  geom_line(size = 1.1) + 
  facet_wrap(~country, nrow=2) +
  scale_x_date(date_breaks="6 month",date_labels="%Y-%m") + 
  labs(x='month', y='reproduction rate') + 
  theme_bw()+ 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        legend.position = "top") +
  scale_color_brewer(palette = "Paired")



#####################
# PSM
#####################
std_country_data <- data.frame(country_data$country, scale(country_data[,c(2:14)])) 

psm_did <- function(did_date, policy, did_range, threshold, distance){
  # did_date <- '2020-09-14'
  
  # data[which(data$date == did_date), c(1, policy)]
  
  tre_list <- data[which(data$date == did_date & data[ ,policy] > threshold), 1]
  con_list <- data[which(data$date == did_date & data[ ,policy] <= threshold), 1]
  if(length(tre_list)<3 | length(tre_list) > 35){
    return(c(length(tre_list),0,0,0,0))
  }
  
  else{
    treatment <- c()
    for(i in c(1:nrow(country_data))){
      if(country_data$country[i] %in% tre_list){
        treatment[i] <- 1
      }
      else{
        treatment[i] <- 0
      }
    }
    treat_data <- data.frame(std_country_data, treatment)
    
    if(FALSE){
      treat_data %>%
        group_by(treatment) %>%
        select(one_of(names(treat_data)[2:14])) %>%
        summarise_all(list(mean = mean, median = median))
      
      lapply(names(treat_data)[2:14], function(v){
        t.test(treat_data[, v] ~ treat_data[, 'treatment'])
      })
    }
    
    
    # m_ps <- glm(treatment ~ ., family = binomial(), data = treat_data[,c(2:5, 7:9, 12:15)])
    # summary(m_ps)
    # prs_df <- data.frame(pr_score = predict(m_ps, type = "response"), treatment = m_ps$model$treatment)
    
    if(FALSE){
      labs <- paste("treatment ", c(0, 1))
      prs_df %>%
        mutate(treatment = ifelse(treatment == 0, labs[1], labs[2])) %>%
        ggplot(aes(x = pr_score)) + 
        geom_histogram(color = "white") + 
        facet_wrap( ~ treatment) + 
        xlab("Probability of going to Catholic school") +
        theme_bw()
    }
    
    
    #mod_match <- matchit(treatment ~ ., method = "optimal", data = treat_data[,c(2:5, 7:9, 12:15)], 
    #distance = distance, caliper=0.2)
    mod_match <- matchit(treatment ~ ., method = "optimal", data = treat_data[,-c(1, 4, 6, 10, 11)], 
                         distance = distance)
    # summary(mod_match)
    # plot(mod_match)
    dta_m <- match.data(mod_match)
    # dim(dta_m)
    
    sub_country_list <- treat_data[as.numeric(row.names(dta_m)),1]
    # sub_country_list
    
    # did
    # did_range <- 30
    sub_treat_data <- data[which(data$country %in% sub_country_list 
                                 & data$date >= as.Date(did_date)-(did_range-1) 
                                 & data$date <= as.Date(did_date)+did_range), 
                           c('country', 'date', 'new_cases_smoothed_per_million')]
    
    treat <- c()
    for(i in c(1:nrow(sub_treat_data))){
      if(sub_treat_data[i,1] %in% tre_list){
        treat[i] <- 1
      }
      else{
        treat[i] <- 0
      }
    }
    
    time <- rep(c(1:(2*did_range)), nrow(sub_treat_data)/(2*did_range))
    dummy <- rep(c(rep(0, did_range), rep(1, did_range)) , nrow(sub_treat_data)/(2*did_range)) 
    time_dummy <- (time-did_range)*dummy
    time_dummy_treat <- (time-did_range)*dummy*treat 
    sub_treat_data <- data.frame(sub_treat_data, treat, time, dummy, time_dummy, time_dummy_treat)
    names(sub_treat_data)[3] <- 'NCSM'
    
    did_model <- lm(NCSM ~ time + treat + time_dummy + time_dummy_treat, data = sub_treat_data)
    
    coef_info <- summary(did_model)$coefficients
    
    ratio <- round(-coef_info[5,1]/(coef_info[2,1]+coef_info[4,1]),4)
    est <- round(coef_info[5,1],4)
    sd <- round(coef_info[5,2],4)
    sign <- round(coef_info[5,4],4)
    
    return(c(length(tre_list), est, sd, sign, ratio))
    
    # panel <- pdata.frame(sub_treat_data, index=c('country','date'))
    # did.reg <- plm(NCSM ~ time + time*dummy + treat + time*treat, 
    # data = panel, model = "within")
    #print(coeftest(did.reg, vcov = function(x) 
    #vcovHC(x, cluster = "group", type = "HC1")))
  }
}

did_date_list <- c('2020-09-14', '2021-02-12', '2021-10-04')
threshold_list <- c(1,1,1,2,1,1,1,2,1,1,2,2,1,1)
policy_list <- c(4:13, 15:17, 19)

did_res <- c(0,0,0,0,0)
for(j in c(1:length(policy_list))){
  for(i in c(1:3)){
    print(c(policy_list[j],did_date_list[i]))
    temp_res <- psm_did(did_date_list[i], policy_list[j], 30, threshold_list[j], 'glm')
    did_res <- rbind(did_res, temp_res)
  }
}
did_res <- rbind(did_res, 
                 psm_did(did_date_list[2], 18, 30, 2, 'glm'))
did_res <- rbind(did_res, 
                 psm_did(did_date_list[3], 18, 30, 4, 'glm'))
did_res <- did_res[c(2:nrow(did_res)), ]
did_res <- data.frame(did_res)
names(did_res) <- c('n', 'estimate', 'sd', 'sign', 'ratio')
write.xlsx(did_res, 'results.xlsx')



###########################
# psm example
###########################
did_date <- "2021-10-04"
i <- 3
j <- 3
tre_list <- data[which(data$date == did_date_list[3] & data[ ,policy_list[j]] > threshold_list[j]), 1]
con_list <- data[which(data$date == did_date_list[3] & data[ ,policy_list[j]] <= threshold_list[j]), 1]

treatment <- c()
for(i in c(1:nrow(country_data))){
  if(country_data$country[i] %in% tre_list){
    treatment[i] <- 1
  }
  else{
    treatment[i] <- 0
  }
}
treat_data <- data.frame(std_country_data, treatment)
# mod_match <- matchit(treatment ~ ., method = "optimal", data = treat_data[,-c(1, 4, 6, 10, 11)])
mod_match <- matchit(treatment ~ ., method = "optimal", data = treat_data[,-c(1, 4, 6, 10, 11)], 
                     distance = 'glm')

# mod_match <- matchit(treatment ~ ., data = treat_data[, -c(1, 4, 6, 10, 11)], method = "nearest", distance = 'glm', caliper = 0.2)

# mod_match <- matchit(treatment ~ ., data = treat_data[, c(2:15)], method = "optimal", distance = 'rpart')
dta_m <- match.data(mod_match)


plot(mod_match, type = "jitter", col = c(2:6), pch = 17, interactive = FALSE)
plot(mod_match, type = c("hist"))
# plot(mod_match, type = "density")
plot(mod_match, type = "ecdf", which.xs = c('population', 'aged_65_older', 'life_expectancy'))
plot(summary(mod_match, subclass = TRUE), var.order = 'unmatched')

sub_country_list <- treat_data[as.numeric(row.names(dta_m)),1] 
did_range <- 30
sub_treat_data <- data[which(data$country %in% sub_country_list 
                             & data$date >= as.Date(did_date)-(did_range-1) 
                             & data$date <= as.Date(did_date)+did_range), 
                       c('country', 'date', 'new_cases_smoothed_per_million')]

treat <- c()
for(i in c(1:nrow(sub_treat_data))){
  if(sub_treat_data[i,1] %in% tre_list){
    treat[i] <- 1
  }
  else{
    treat[i] <- 0
  }
}

time <- rep(c(1:(2*did_range)), nrow(sub_treat_data)/(2*did_range))
dummy <- rep(c(rep(0, did_range), rep(1, did_range)) , nrow(sub_treat_data)/(2*did_range))
time_dummy <- (time-30)*dummy
time_dummy_treat <- (time-30)*dummy*treat

sub_treat_data <- data.frame(sub_treat_data, treat, time, dummy, time_dummy, time_dummy_treat)
names(sub_treat_data)[3] <- 'NCSM'

did_model <- lm(NCSM ~ time + treat + time_dummy + time_dummy_treat, data = sub_treat_data)

summary(did_model)$coefficients
summary(did_model)
coef_info <- summary(did_model)$coefficients[,1]

time_ex <- c(c(1:60), c(1:60))
dummy_ex <- rep(c(rep(0,30),rep(1,30)),2)
treat_ex <- c(rep(0,60),rep(1,60))
time_dummy_ex <- (time_ex-30)*dummy_ex
time_dummy_treat_ex <- (time_ex-30)*dummy_ex*treat_ex

y <- coef_info[1] + time_ex*coef_info[2] + treat_ex*coef_info[3] + time_dummy_ex*coef_info[4] + time_dummy_treat_ex*coef_info[5]
y1 <- y[1:60]
y2 <- y[61:120]
y3 <- (coef_info[1] + time_ex*coef_info[2] + treat_ex*coef_info[3] + time_dummy_ex*coef_info[4])[61:120]

did_ex_data <- data.frame(y = c(y1,y2,y3), date = rep(sub_treat_data$date[1:60],3), 
                          group = c(rep('control',60), rep('treatment',60), rep('treatment (counterfactual)',60)))
ggplot(data = did_ex_data, aes(x=date, y=y, group=group, color=group, shape=group)) +
  geom_line(size = 1.2) + geom_point(size=2) + 
  geom_vline(xintercept = as.Date('2021-10-04'), linetype="dotted", color = "black", size=1.2) + 
  scale_color_brewer(palette = "Accent") + 
  theme_bw() + theme(legend.position = "top") + labs(y = 'new cases smoothed per million') + 
  theme(axis.title.x=element_text(size=14), 
        axis.title.y=element_text(size=14), 
        legend.text=element_text(size=12))+ 
  scale_x_date(date_breaks="10 day",date_labels="%Y-%m-%d") + 
  labs(x='date')

