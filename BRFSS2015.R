suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))

BRFSS <- read_csv("BRFSS2015.csv")

#Q1: How many people have any kind of health coverage
Q1 <- BRFSS %>% 
      filter(HLTHPLN1 == "1") %>%
      count()

#Q2 finding the average number of days for poor mental health
BRFSS2 <- BRFSS %>%
        mutate(MENTHLTH = if_else(MENTHLTH == 88, 0, MENTHLTH))
Q2 <- BRFSS2 %>%
        filter(MENTHLTH < 31) %>%
        summarize("Average number of days mental health not good" = mean(MENTHLTH))
#Q3 Mean and Sd for weight grouped by having arthritis or not
Q3 <-  BRFSS %>%
  filter(HAVARTH3 != 7, HAVARTH3 != 9, WEIGHT2 != 9999, WEIGHT2 != 999, WEIGHT2 != 7777, !is.na(WEIGHT2), !is.na(HAVARTH3)) %>%
  group_by(HAVARTH3)%>%
  select(HAVARTH3, WEIGHT2)%>%
  mutate(WEIGHT2 = ifelse(WEIGHT2 >= 9000, (WEIGHT2 - 9000) * 2.20462, WEIGHT2))%>%
  summarize(mean_weight = round(mean(WEIGHT2),2), sd_weight = round(sd(WEIGHT2),2))%>%
  as.data.frame()

#Q4 Removing Outliers for total number of minutes physically active for further analysis 
upperm <- quantile(BRFSS$PA1MIN_, 0.997, na.rm = TRUE)
lowerm <- quantile(BRFSS$PA1MIN_, 0.003, na.rm = TRUE)
minout <- which(BRFSS$PA1MIN_ > upperm | BRFSS$PA1MIN_ < lowerm)
noout <- BRFSS[-minout,]
Q4 <-round((nrow(BRFSS) - length(minout)) /nrow(BRFSS) *100, 2)

#Q5
Q5 <- noout %>%
  group_by(MARITAL)%>%
  filter(!is.na(PA1MIN_))%>%
  summarize(round(mean(PA1MIN_),2),round(sd(PA1MIN_),2),round(min(PA1MIN_),2),round(max(PA1MIN_),2))%>%
  as.data.frame()

#Q6

MARITALchr = as.character(noout$MARITAL)
ggplot(noout, aes(x=MARITALchr, y=PA1MIN_)) +
geom_boxplot()

#Q7

Exercise <- lm(PA1MIN_ ~ MARITAL, data = noout)
Q7 <- summary(Exercise)

#Q8

model <- aov(PA1MIN_ ~ MARITALchr, data=noout)
Q8 <- TukeyHSD(model)

#Q9

Exercise_fruit <- lm(PA1MIN_ ~ MARITAL + _FRUTSUM, data = noout)







