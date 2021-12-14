df=read.csv("Téléchargements/gold history/Gold_Daily .csv")
library(ggplot2)
library(tidyverse)

install.packages("remotes")
remotes::install_github("Displayr/flipTime")

library(flipTime)

AsDate(df$Date)

head(df)

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y')) %>%
  ggplot(aes(x = Year , y = Price, color=Year))+
  geom_point(aes(x = Year , y = Price, color=Year)) +
  ggtitle("Evolution du prix de l'or en fonction des années") 

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y')) %>%
  filter(Price>1500)%>%
  ggplot(aes(x = Year , y = Price, color=Year))+
  geom_point(aes(x = Year , y = Price, color=Year)) +
  ggtitle("Evolution du prix de l'or en fonction des années") 

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y')) %>%
  filter(Price<300) %>%
  ggplot(aes(x = Year , y = Price, color=Year))+
  geom_point(aes(x = Year , y = Price, color=Year)) +
  ggtitle("Evolution du prix de l'or en fonction des années") 

df %>%
  arrange(Low) %>%
  slice(1:10) %>%
  ggplot(aes(x = Date , y = Low, color=Date))+
  geom_point(aes(x = Date , y = Low, color=Date)) +
  geom_line(aes(x = Date , y = Low, color=Date, group=1))+
  ggtitle("Evolution du prix de l'or en fonction des années") 

df %>%
  arrange(desc(High)) %>%
  slice(1:10) %>%
  ggplot(aes(x = Date , y = High, color=Date))+
  geom_point(aes(x = Date , y = High, color=Date)) +
  geom_line(aes(x = Date , y = High, color=Date, group=1))+
  ggtitle("Evolution du prix de l'or en fonction des années")
  

summary(df)

# df %>%
# mutate(df, start_end=Year.Close - Year.Open, pos= start_end>0) %>%
# group_by(pos) %>%
# ggplot(aes(x=Year, y=start_end)) +  
# geom_point(aes(x=Year, y=start_end, color=pos)) +
# geom_col(aes(x=Year, y=start_end, color=pos)) +
# ggtitle("Ce graphe représente la différence entre le prix de l'or à la fin de l'année et le prix de l'or au début de l'année ")  
# 
# df %>% 
#   arrange(desc(Year.High)) %>%
#   slice(1:10) %>%
#   ggplot(aes(x=Year, y=Year.High))+
#   geom_point(aes(x=Year, y=Year.High))+
#   geom_line(aes(x=Year, y=Year.High))+
#   ggtitle("Ce graphe représente les dix valuers maxiamles de l'or en dollar")
# 
# df$Annual...Change
# df %>%
#   mutate(pos_annual_change=abs(Annual...Change), pos_values=Annual...Change>0) %>%
#   arrange(desc(pos_annual_change)) %>%
#   slice(1:10) %>%
#   ggplot(aes(x=Year, y=pos_annual_change))+
#   geom_point(aes(x=Year, y=Annual...Change))+
#   geom_col(aes(x=Year, y=Annual...Change,  fill=pos_values))
# ggtitle("les plus grandes fluctuations dans l'histoire de l'or")


