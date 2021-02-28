library(tidyverse)
library(ggplot2)
library(janitor)

"fdi.csv" %>% 
  read.csv() -> raw_data

c("AGRICULTURAL MACHINERY", "SUGAR", "FOOD PROCESSING INDUSTRIES",
  "VEGETABLE OILS AND VANASPATI", "AGRICULTURE SERVICES") -> services_required

raw_data[raw_data$Sector %in% services_required, ] %>% 
  select(c(1,16,17,18))%>% 
  pivot_longer(cols = c(-1), names_to = "year", values_to = "values") %>% 
  separate(year, into=c("metric","date"), sep = 1)-> services_raw_data

gsub("[.]", "-", services_raw_data$date) -> services_raw_data$date

services_raw_data %>% 
  select(-2) %>% 
  clean_names()-> services_cleaned_data

services_cleaned_data$sector <- factor(services_cleaned_data$sector, levels=rev(unique(services_cleaned_data$sector)))
services_cleaned_data$date <- factor(services_cleaned_data$date, levels=rev(unique(services_cleaned_data$date)))
services_cleaned_data 

library(ggplot2)
ggplot(services_cleaned_data , aes(x=date, y=values, fill=sector))+
  geom_col(position = "dodge")+
  scale_fill_brewer(palette="Accent")+
  scale_fill_manual(values=c("#b19cc2","#4c6e9b","#84b3cd","#98ced0","#eab5b9"))+
  ggtitle("FDI Equity Inflows in Agriculture")+
  theme_linedraw()+
  theme_light()+
  theme(legend.position="top",
        legend.title = element_blank(),
        plot.title = element_text(vjust = 1.0),
        panel.border = element_blank())+
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  labs(y="US $ million")+
  ylim(0,800)
