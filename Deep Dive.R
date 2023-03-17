install.packages("sqldf")

library(sqldf)
library(stringr)
library(dplyr)

table(my_data$SEC_CONTRIBUTORY_CAUSE)
# Which road might need extra police force to direct people direcion

my_data <- read.csv("/Users/zhuguanyu/Desktop/Traffic_Crashes_-_Crashes (1).csv")

result0 <- sqldf("SELECT STREET_NAME, STREET_NO, STREET_DIRECTION, COUNT(CRASH_RECORD_ID), TRAFFICWAY_TYPE FROM my_data WHERE PRIM_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY'
                and (SEC_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY' or SEC_CONTRIBUTORY_CAUSE == 'NOT APPLICABLE' or SEC_CONTRIBUTORY_CAUSE == 'UNABLE TO DETERMINE' or 
                SEC_CONTRIBUTORY_CAUSE == 'IMPROPER LANE USAGE' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING TRAFFIC SIGNALS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING YIELD SIGN' or 
                SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING ROAD MARKINGS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING STOP SIGN' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING OTHER TRAFFIC SIGNS' or
                SEC_CONTRIBUTORY_CAUSE == 'FAILING TO YIELD RIGHT-OF-WAY' or SEC_CONTRIBUTORY_CAUSE =='VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)'
                or SEC_CONTRIBUTORY_CAUSE == 'IMPROPER TURNING/NO SIGNAL' or SEC_CONTRIBUTORY_CAUSE =='ROAD CONSTRUCTION/MAINTENANCE' or SEC_CONTRIBUTORY_CAUSE == 'OBSTRUCTED CROSSWALKS'
                or SEC_CONTRIBUTORY_CAUSE =='ROAD ENGINEERING/SURFACE/MARKING DEFECTS' or SEC_CONTRIBUTORY_CAUSE == 'RELATED TO BUS STOP') 
                GROUP BY STREET_NAME, STREET_NO 
                ORDER BY COUNT(CRASH_RECORD_ID) DESC")


result1 <- sqldf("SELECT STREET_NAME, STREET_NO, COUNT(CRASH_RECORD_ID), TRAFFICWAY_TYPE FROM my_data WHERE PRIM_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY'
                and (SEC_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY' or SEC_CONTRIBUTORY_CAUSE == 'NOT APPLICABLE' or SEC_CONTRIBUTORY_CAUSE == 'UNABLE TO DETERMINE' or 
                SEC_CONTRIBUTORY_CAUSE == 'IMPROPER LANE USAGE' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING TRAFFIC SIGNALS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING YIELD SIGN' or 
                SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING ROAD MARKINGS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING STOP SIGN' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING OTHER TRAFFIC SIGNS' or
                SEC_CONTRIBUTORY_CAUSE == 'FAILING TO YIELD RIGHT-OF-WAY' or SEC_CONTRIBUTORY_CAUSE =='VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)'
                or SEC_CONTRIBUTORY_CAUSE == 'IMPROPER TURNING/NO SIGNAL') and (STREET_NAME == '63RD ST')
                GROUP BY STREET_NAME, STREET_NO 
                ORDER BY COUNT(CRASH_RECORD_ID) DESC")




year <- substring(my_data$CRASH_DATE, 7, 10)


my_data$year <- year


result2 <-  sqldf("SELECT CRASH_MONTH, year, COUNT(CRASH_RECORD_ID), SUM(INJURIES_TOTAL), SUM(INJURIES_FATAL) 
                  FROM my_data GROUP BY CRASH_MONTH, year ORDER BY year, CRASH_MONTH")


result2$TIME <- paste0(result2$CRASH_MONTH, '_', result2$year)


# Western Avenue
Western_AVE <- sqldf("SELECT PRIM_CONTRIBUTORY_CAUSE FROM my_data WHERE STREET_NAME == 'WESTERN AVE' AND INJURIES_TOTAL > 0")
Western_AVE <- as.data.frame(table(Western_AVE)) 
Western_AVE <- arrange(Western_AVE, desc(Freq))
Western_AVE <- Western_AVE[2:6,]


# Cicero Avenue
Cicero_AVE <- sqldf("SELECT PRIM_CONTRIBUTORY_CAUSE FROM my_data WHERE STREET_NAME == 'CICERO AVE' AND INJURIES_TOTAL > 0")
Cicero_AVE <- as.data.frame(table(Cicero_AVE)) 
Cicero_AVE <- arrange(Cicero_AVE, desc(Freq))
Cicero_AVE <- Cicero_AVE[2:6,]

# Pulaski Road
Pulaski_Road <- sqldf("SELECT PRIM_CONTRIBUTORY_CAUSE FROM my_data WHERE STREET_NAME == 'PULASKI RD' AND INJURIES_TOTAL > 0")
Pulaski_Road <- as.data.frame(table(Pulaski_Road)) 
Pulaski_Road <- arrange(Pulaski_Road, desc(Freq))
Pulaski_Road <- Pulaski_Road[2:7,]
Pulaski_Road <- Pulaski_Road[-5,]



library(openxlsx)
write.xlsx(result2, "/Users/zhuguanyu/Desktop/month_plot.xlsx", rowNames = FALSE)
write.xlsx(Pulaski_Road , "/Users/zhuguanyu/Desktop/Pulaski_Road.xlsx", rowNames = FALSE)
write.xlsx(Cicero_AVE, "/Users/zhuguanyu/Desktop/Cicero_AVE.xlsx", rowNames = FALSE)
write.xlsx(Western_AVE, "/Users/zhuguanyu/Desktop/Western_Avenue.xlsx", rowNames = FALSE)
write.xlsx(time_df , "/Users/zhuguanyu/Desktop/time_df.xlsx", rowNames = FALSE)



test <- sqldf("SELECT STREET_NAME, STREET_NO, COUNT(CRASH_RECORD_ID), TRAFFICWAY_TYPE FROM my_data WHERE PRIM_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY'
                and (SEC_CONTRIBUTORY_CAUSE == 'DRIVING ON WRONG SIDE/WRONG WAY' or SEC_CONTRIBUTORY_CAUSE == 'NOT APPLICABLE' or SEC_CONTRIBUTORY_CAUSE == 'UNABLE TO DETERMINE' or 
                SEC_CONTRIBUTORY_CAUSE == 'IMPROPER LANE USAGE' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING TRAFFIC SIGNALS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING YIELD SIGN' or 
                SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING ROAD MARKINGS' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING STOP SIGN' or SEC_CONTRIBUTORY_CAUSE == 'DISREGARDING OTHER TRAFFIC SIGNS' or
                SEC_CONTRIBUTORY_CAUSE == 'FAILING TO YIELD RIGHT-OF-WAY' or SEC_CONTRIBUTORY_CAUSE =='VISION OBSCURED (SIGNS, TREE LIMBS, BUILDINGS, ETC.)'
                or SEC_CONTRIBUTORY_CAUSE == 'IMPROPER TURNING/NO SIGNAL') 
                GROUP BY STREET_NAME
                ORDER BY COUNT(CRASH_RECORD_ID) DESC")


time_df <- as.data.frame(table(my_data$CRASH_HOUR))


################################################
library(lubridate)
library(doMC)
registerDoMC(2)
library(ggplot2)
library(dplyr)
install.packages('ggthemes')
library(ggthemes)
library(RColorBrewer)
library(skimr)
library(rgdal)
library(viridis)


df <- read.csv("/Users/zhuguanyu/Desktop/Traffic_Crashes_-_Crashes (1).csv")


df$CRASH_MONTH_NAME <- sapply(df$CRASH_MONTH, function(x) month.name[x])

df$CRASH_DATE_YEAR <- sapply(df$CRASH_DATE, function(x) strsplit(x," ")[[1]][1])
df$CRASH_DATE_YEAR <- as.Date(df$CRASH_DATE_YEAR, format="%m/%d/%Y")
df$CRASH_DATE_YEAR_NUM <- lubridate::year(df$CRASH_DATE_YEAR)

df_monthly_crash <- data.frame(df %>% group_by(CRASH_DATE_YEAR_NUM, CRASH_MONTH_NAME) %>% summarise(count=n()))

df_daily_crash <- data.frame(df %>% group_by(CRASH_DATE_YEAR) %>% summarise(count=n()))
df_daily_crash$month <- lubridate::month(df_daily_crash$CRASH_DATE_YEAR)
df_daily_crash$month_name <- sapply(df_daily_crash$month, function(x) month.name[x])


df_daily_crash %>% 
  mutate(
    month_name_ordered = factor(month_name, month.name),
    current_year = lubridate::year(CRASH_DATE_YEAR)) %>% 
  ggplot(aes(x=month_name_ordered ,y=count)) + 
  geom_boxplot(alpha=.25) + 
  geom_jitter(aes(color=factor(current_year)), size=1) +
  theme_fivethirtyeight(12) + labs(title="daily crash count") +
  scale_color_brewer(name="", palette='Paired')



