df=read.csv("Téléchargements/gold history/Gold_Yearly .csv")
library(ggplot2)
library(tidyverse)
ggplot(data=df ,aes(x=Year, y=Year.High, color="red",fill="red"))+
geom_point()+ 
geom_line()+
ggtitle("Ce graphe montre l'évolution du prix de l'or selon les différentes années")  
summary(df)

df %>%
mutate(df, start_end=Year.Close - Year.Open, pos= start_end>0) %>%
group_by(pos) %>%
ggplot(aes(x=Year, y=start_end)) +  
geom_point(aes(x=Year, y=start_end, color=pos)) +
geom_col(aes(x=Year, y=start_end, color=pos)) 
ggtitle("Ce graphe représente la différence entre le prix de l'or à la fin de l'année et le prix de l'or au début de l'année ")  
