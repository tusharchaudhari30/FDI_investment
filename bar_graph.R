library(tidyverse)
library(janitor)

"abc.csv" %>% 
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
services_cleaned_data %>% 
  ggplot(aes(x=sector, y=values, fill=date)) +
  geom_bar(stat="identity", width = 0.75, position = position_dodge(width=0.8)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 200),
                     name = "US $ Million") +
  scale_x_discrete(name ="Sector") +
  scale_fill_discrete(breaks = rev(levels(services_cleaned_data$date))) +
  scale_fill_manual(values=c("#1ce8b2", "#f5cf14","#e0453a" ))+
  theme(legend.position="top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        panel.border = element_blank()) +
  labs(title = "FDI Equality Inflows in Agriculture from 2014-15 to 2016-17")