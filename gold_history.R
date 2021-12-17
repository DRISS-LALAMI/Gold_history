#------------------------------ Read the dataser -----------------------------------------------------
df=read.csv("Gold_Daily .csv")

#------------------------------ Import some necessary libs ------------------------------------------------
library(ggplot2)
library(tidyverse)
remotes::install_github("Displayr/flipTime")
library(flipTime)


#--------------------------------- Some  Infos of the dataframe ------------------------------------
head(df)
summary(df)
str(df)

#Convertion du prix du once vers le gramme ----------
a=lapply(df$Price, `/`, 28.3495)
b=as.numeric(sprintf("%.1f", a))

#------------------- Plot 1 : Evolution of the Gold's Price ---------------------------------------------------

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y'),Price_gramme=b ) %>%
  ggplot(aes(x = Year , y = Price_gramme, color=Year))+
  geom_point(aes(x = Year , y = Price_gramme, color=Year)) +
  theme(legend.position = "none")
  #+ggtitle("Evolution du prix de l'or pour un gramme en fonction des années") 


#------------------- Plot 2 : Price per grame > 50----------------------------------------------------

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y'), Price_gramme=b) %>%
  filter(Price_gramme>50)%>%
  ggplot(aes(x = Year , y = Price_gramme, color=Year))+
  geom_point(aes(x = Year , y = Price_gramme, color=Year)) +
  theme(legend.position = "none")
  #+ggtitle("les années où l'or a dépassé 50 dollar le gramme") 

#----------------------- Plot 3 : les années où l'or a chuté à moins de 15 dollar le grame -----------------

df %>%
  mutate(Year=format(AsDate(df$Date) , format='%Y'), Price_gramme=b) %>%
  filter(Price_gramme<15) %>%
  ggplot(aes(x = Year , y = Price_gramme, color=Year))+
  geom_point(aes(x = Year , y = Price_gramme, color=Year)) +
  theme(legend.position = "none")
  # + ggtitle("les années où l'or a chuté à moins de 15 dollar le grame") 

#Convertion du prix bas du once vers le gramme ---------
c=lapply(df$Low, `/`, 28.3495)
d_Low=as.numeric(sprintf("%.1f",c ))


#-------------------------- Plot 4 : les dix valeurs les plus basses de l'or pour le gramme ---------------------------

df %>%
  mutate(price_low=d_Low) %>%
  arrange(price_low) %>%
  slice(1:8) %>%
  ggplot(aes(x = Date , y = price_low, color="edges"))+
  geom_point(aes(x = Date , y = price_low, color="edges")) +
  geom_col(aes( x = Date, y=price_low, fill="overall", width=0.8))+
  scale_fill_manual(
    values = c("overall"= "#FF2200"))+
  scale_color_manual(
    values = c("edges"= "black"))+
  geom_text(aes(label = price_low), vjust = -0.5) +
  theme(legend.position = "none")
  # + ggtitle("les dix valeurs les plus basses de l'or pour le gramme")

#Convertion du prix bas du once vers le gramme ----
e=lapply(df$High, `/`, 28.3495)
f=as.numeric(sprintf("%.1f",e ))

#--------------------- Plot 5 : Les dix valeurs les plus hautes de l'or pour le gramme-------------

df %>%
  mutate(price_high=f) %>%
  arrange(desc(price_high)) %>%
  slice(1:10) %>%
  ggplot(aes(x = Date , y = price_high, color= "edges"))+
  geom_col(aes( x = Date, y=price_high, fill="overall", width=0.8))+
  scale_fill_manual(
  values = c("overall"= "#7FFF00"))+
  scale_color_manual(
    values = c("edges"= "black"))+
  geom_text(aes(label = price_high), vjust = -0.5)+
  theme(legend.position = "none")
  # + ggtitle("Les dix valeurs les plus hautes de l'or pour le gramme")
  


#------------- Plot 6 : Les dix plus grand changement dans l'histoire de l'or ------------

legend_title="legend_gold"

df %>%
  mutate(positive= Change..>0 , abs_value= abs(Change..)) %>%
  arrange(desc(abs_value)) %>%
  slice(1:10) %>%
  group_by(positive) %>%
  ggplot(aes(x = Date , y = Change.. ,fill=positive))+
  geom_col(aes(x = Date , y = Change.., fill=positive ))+
  geom_text(aes(label = Change..), vjust = -0.5)+
  scale_fill_manual(legend_title,values=c("#FF2200","#7CFF00"),
                    labels = paste(c("drop of price", "raise of price")))
  #+ggtitle("Les dix plus grand changement dans l'histoire de l'or")





#----------------- Plot 7 : Les dix jours qui ont connus les plus grand volume de transaction --------------²

df$Vol
new_vol<- as.numeric(gsub("K","",as.character(df$Vol)))

volume_title="volume de transactions"

df %>%
  mutate(nv_vol=new_vol) %>%
  arrange(desc(nv_vol)) %>%
  slice(1:10) %>%
  ggplot(aes(x = Date , y = nv_vol))+
  geom_col(aes(x = Date , y = nv_vol,fill="overall" ))+
  geom_text(aes(label = nv_vol), vjust = -0.5)+
  scale_fill_manual(volume_title,values=c("overall" ="#1a5276"),
                    labels = paste(c("volume")))                  
  #+ ggtitle("Les dix jours qui ont connus les plus grand volume de transaction")


  

  
