library(tidyverse)
library(ggplot2)
trace(grDevices:::png, quote({
  if (missing(type) && missing(antialias)) {
    type <- "cairo-png"
    antialias <- "subpixel"
  }
}), print = FALSE)
read_csv("fdi.csv") %>% t() %>% data.frame()->df
colnames(df) = df[1, ] 
df = df[-1, ]
df<-df %>% rownames_to_column(var="Year")

df %>% select("Year","AGRICULTURE SERVICES","AGRICULTURAL MACHINERY") %>% 
  rename("ag_ser" = "AGRICULTURE SERVICES","ag_mac" = "AGRICULTURAL MACHINERY") %>% 
  mutate(ag_ser = as.double(ag_ser)) %>% 
  mutate(ag_mac = as.double(ag_mac)) ->df

ggplot(df,aes(x=Year, group=1))+
  geom_line(size=1,aes(y=ag_ser,color="#4cbbab"))+
  geom_point(size=2,aes(y=ag_ser,color="#4cbbab"))+
  geom_line(size=1,aes(y=ag_mac,color="#e76f7d"))+
  geom_point(size=2,aes(y=ag_mac,color="#e76f7d"))+
  ggtitle("Trend Of Agriculture Services And Agriculture Machinary In FDI Equity Inflows")+
  theme_linedraw()+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5,size = 15),
        panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(axis.text.x = element_text(angle = 40,hjust = 1,size = 10))+
  theme(legend.position = "top",legend.title = element_blank())+
  guides(fill = guide_legend(override.aes = list(shape = 22)),
         color = guide_legend(reverse = TRUE))+
  labs(y="US $ million")+
  scale_colour_manual(name = 'sector', 
                      values =c('#4cbbab'='#4cbbab','#e76f7d'='#e76f7d'), 
                      labels = c('AGRICULTURE MACHINERY','AGRICULTURE SERVICES'))

