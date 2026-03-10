# Event studies by state

library(pacman)
p_load(tidyverse, stringr, viridis, reshape2, jtools,
       data.table, dtplyr, lubridate, plm, estimatr, fixest)

# Load in data

df <- fread("Data//scanner_with_past.csv") %>% as.data.frame()

df <- df %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7)
) 

df<- df[order(df$fips, df$week_end),]



# aggregate Event study -------------------------------------------------------
L = length(df$fips)
event_df <- df[0,]

for(i in 1:L){
  
  if(df$Landfall[i] == 1){
    test_df <- df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- df %>% filter(df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev_per_cap ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8))
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0
)

ES_results <- rbind(ES_results, ref_point)


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))+
  labs(title = "", x = "Time", y = "Total Revenue per 100K Residents")



# get data for last hit accurate or not and plot event studies. ---------------
df <- subset(df, !is.na(last_anyTG_Hit))

# accurate last forecast 

L = length(df$fips)
event_df <- df[0,]



for(i in 1:L){
  
  if(df$Landfall[i] == 1 && df$last_anyTG_Hit[i] == 1){
    test_df <- df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- df %>% filter(df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev_per_cap ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  Forecast = "True"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  Forecast = "True"
)

ES_results <- rbind(ES_results, ref_point)

Results_Forecasts <- ES_results


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study of Accurate Forecasts", x = "Time", y = "Total Revenue per 100K Residents")




# False Forecasts

L = length(df$fips)
event_df <- df[0,]



for(i in 1:L){
  
  if(df$Landfall[i] == 1 && df$last_anyTG_NoHit[i] == 1){
    test_df <- df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- df %>% filter(df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev_per_cap ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  Forecast = "False"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  Forecast = "False"
)

ES_results <- rbind(ES_results, ref_point)

Results_Forecasts <- rbind(Results_Forecasts, ES_results)


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 20), axis.text = element_text(size = 20),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Inaccurate Forecasts", x = "Time", y = "Total Revenue per 100K Residents")



ggplot(data = Results_Forecasts, aes(x = time, y = estimate, color = Forecast))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Forecasts", x = "Time", y = "Total Revenue per 100K Residents")



# Get counties by mean false forecast streak  ----------------------------------

df <- fread("Data//scanner_with_past.csv") %>% as.data.frame()

df <- df %>% mutate(
  fips = str_pad(fips, 5,'left', '0'),
  year = str_sub(week_end, end = 4),
  month = str_sub(week_end, start =6, end = 7),
  acc_streak = ifelse(hit_streak > 0, hit_streak, -1*miss_streak)
) 

df<- df[order(df$fips, df$week_end),]


hur_event <- subset(df, Landfall == 1 | anyTG == 1)
hur_event <- subset(hur_event, !is.na(acc_streak))

streaks_df <- hur_event %>% group_by(fips) %>% summarize(
  max_acc = max(acc_streak, na.rm = T),
  min_acc = min(acc_streak, na.rm=T),
  mean_acc = mean(acc_streak, na.rm=T)
)

summary(streaks_df$mean_acc)

q1 <- subset(streaks_df, mean_acc < -14)$fips
q2 <- subset(streaks_df, mean_acc >= -14 & mean_acc < -8.4)$fips
q3 <- subset(streaks_df, mean_acc >=-8.4 & mean_acc < -3.8)$fips
q4 <- subset(streaks_df, mean_acc >= -3.8)$fips

# Event study by accuracy quartiles --------------------------------------

q1_df <- subset(df, fips %in% q1)

L = length(q1_df$fips)
event_df <- q1_df[0,]



for(i in 1:L){
  
  if(q1_df$Landfall[i] == 1){
    test_df <- q1_df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- q1_df %>% filter(q1_df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev_per_cap ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  quartile = "1"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  quartile = "1"
)

ES_results <- rbind(ES_results, ref_point)

All_Quartiles <- ES_results


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Quartile 1", x = "Time", y = "Total Revenue per 100K Residents")







# q2


q2_df <- subset(df, fips %in% q2)

L = length(q2_df$fips)
event_df <- q2_df[0,]



for(i in 1:L){
  
  if(q2_df$Landfall[i] == 1){
    test_df <- q2_df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- q2_df %>% filter(q2_df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev_per_cap ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  quartile = "2"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  quartile = "2"
)

ES_results <- rbind(ES_results, ref_point)

All_Quartiles <- rbind(All_Quartiles, ES_results)


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Quartile 2", x = "Time", y = "Total Revenue per 100K Residents")


# q3
q3_df <- subset(df, fips %in% q3)

L = length(q3_df$fips)
event_df <- q3_df[0,]



for(i in 1:L){
  
  if(q3_df$Landfall[i] == 1){
    test_df <- q3_df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- q3_df %>% filter(q3_df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  quartile = "3"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  quartile = "3"
)

ES_results <- rbind(ES_results, ref_point)

All_Quartiles <- rbind(All_Quartiles, ES_results)


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Quartile 3", x = "Time", y = "Total Revenue per 100K Residents")


# q4
q4_df <- subset(df, fips %in% q4)

L = length(q4_df$fips)
event_df <- q4_df[0,]



for(i in 1:L){
  
  if(q4_df$Landfall[i] == 1){
    test_df <- q4_df[i,]
    week <- as.Date(test_df$week_end)
    fip_now <- test_df$fips
    county <- q4_df %>% filter(q4_df$fips == fip_now)
    result <- county %>% filter(week_end >= week-70 & week_end <= week+70 )
    result <- result %>% mutate(
      ref_num = (as.Date(week_end) - week)/7
    )
  }else{next}
  
  event_df <- rbind(event_df, result)
}

event_df$ref_num = relevel(as.factor(event_df$ref_num), ref = "-2")

# run event study
ES = feols(data = event_df, total_rev ~ ref_num | fips + year + month, cluster = event_df$fips)
results= tidy(ES)     

test <- confint(ES, level =0.95)

#get 95% confidence intervals
ES_results = results %>% mutate(
  lci = confint(ES, level = 0.95)$'2.5 %',
  uci = confint(ES, level = 0.95)$'97.5 %',
  time = as.integer(str_sub(term, start = 8)),
  quartile = "4"
)

ref_point <- results [1,]
ref_point <- ref_point %>% mutate(
  term = "ref_num-2",
  estimate = 0,
  std.error=0,
  time = -2,
  lci = 0,
  uci = 0,
  quartile = "4"
)

ES_results <- rbind(ES_results, ref_point)

All_Quartiles <- rbind(All_Quartiles, ES_results)


#plot event study
ggplot(data = ES_results, aes(x = time, y = estimate))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study for Quartile 4", x = "Time", y = "Total Revenue per 100K Residents")




ggplot(data = All_Quartiles, aes(x = time, y = estimate, color = quartile))+
  geom_line()+
  geom_point()+
  theme_minimal()+
  #geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2)+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_hline(yintercept = 0)+
  geom_point(aes(x = -2, y = 0), fill = "white", shape = 21, size = 2)+
  theme(axis.title = element_text(size = 15), axis.text = element_text(size = 10),
        plot.title = element_text(size = 30))+
  labs(title = "Event Study by Quartile", x = "Time", y = "Total Revenue per 100K Residents")
