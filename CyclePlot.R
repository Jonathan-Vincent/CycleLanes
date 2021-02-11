require(ggplot2)
library(scales)
library(readr)
library(tidyr)
library(dplyr)
library(cowplot)

col_types1 <- cols(
  City = col_character(),
  Date = col_datetime(format = ""),
  Road = col_double(),
  Cycle = col_double(),
  Ratio = col_double(),
  Total = col_double(),
  Cycleway = col_double(),
  NoVehicle1 = col_double(),
  NoVehicle2 = col_double(),
  Designated = col_double(),
  Lane = col_double(),
  LeftLane = col_double(),
  RightLane = col_double()
)

col_types2 <- cols(
  City = col_character(),
  Road = col_double(),
  Cycle = col_double(),
  Ratio = col_double(),
  Total = col_double(),
  Cycleway = col_double(),
  NoVehicle1 = col_double(),
  NoVehicle2 = col_double(),
  Designated = col_double(),
  Lane = col_double(),
  LeftLane = col_double(),
  RightLane = col_double()
)
  
historic <- read_csv("CycleLanes/historic.csv",col_types = col_types1)

historic$Date = as.Date(historic$Date)

#Set the denominator to equal the most recent road length (2021)
#This better reflects actual cycle lane growth
historic <- historic %>%
  group_by(City) %>%
  mutate(FixedRatio = 100*(Cycle)/first(Road))

#Remove data before 2013
historic = historic[historic$Date > as.Date("2012-12-31"),]

#Remove Cardiff, just clutters the graph
historic <- historic[!historic$City %in% c('Cardiff'),]

cycle <- read_csv("CycleLanes/current.csv",col_types = col_types2)
cycle$City[cycle$City=='ParisMetropolitan'] = 'Paris*'

#Extract Dutch national average
NL <- 100*cycle$Ratio[cycle$City=='Netherlands']
#remove uninteresting cities
cycle <- cycle[!cycle$City %in% c('Utrecht','The Hague','Rotterdam','Eindhoven',
                                  'Groningen','Madrid','Rome','Zurich','Netherlands'),]

#Denoted highway=cycleway on OpenStreetMaps
cycle$`Segregated Cycleways` = cycle$Cycleway/cycle$Road

#Denoted bicycle:lane, bicycle:left or bicycle:right on OpenStreetMaps
cycle$`Roadside Cycle Lanes` = (cycle$Lane + cycle$LeftLane + cycle$RightLane)/cycle$Road

#Other infrastructure, denoted by [bicycle=designated], [vehicle=no] or [motor_vehicle=no]
cycle$Other = (cycle$Cycle - cycle$Cycleway -cycle$Lane - cycle$LeftLane - cycle$RightLane)/cycle$Road

#To highlight cities that appear on the left and right
bold.labels <- rep("plain",length(cycle$City))
bold.labels[c(1,7,11,13,19)] = 'bold'

cycleLong <- gather(cycle,
                 Condition,
                 Value,
                 `Segregated Cycleways`:Other)

breaksp = seq(0, 22, by = 2)
labsize = 1.2
labpad = unit(0.05, "lines")

#Construct barplot on the right
p<-ggplot(data=cycleLong, aes(y=reorder(City,Value),x=100*Value)) +
  geom_bar(aes(fill = Condition),stat="identity")+
  geom_vline(xintercept = NL, linetype="longdash", color = "orange", size=0.25)+
  geom_label(aes(x=17,y="London",label="Dutch\nNational\nAverage"),colour='black',
             fill='white', size=labsize,label.size = NA,label.padding = labpad,hjust=0,
             lineheight = 0.8)+
  theme_minimal()+
  ylab(element_blank())+
  xlab(element_blank())+
  scale_x_continuous(expand = c(0,0),breaks=breaksp,limits = c(0,22.1))+
  scale_y_discrete(expand = expansion(add = c(0, 0)))+
  scale_fill_manual(values=c('#00ffff','#01a1c7','#304c6d'))+
  labs(subtitle="By European city, February 2021")+
  theme(legend.title = element_blank(),
        legend.position='top',
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(size=0.2),
        axis.ticks = element_blank(),
        axis.ticks.length.x = unit(0, "lines"),
        axis.text.y = element_text(colour='black',margin = unit(c(0, -0.5, 0, 0), "mm"),hjust=1,face = bold.labels),
        axis.text.x = element_text(colour='black',margin = unit(c(0.1, 0, 0, 0), "mm"),vjust=1),
        text = element_text(size = 4),
        plot.margin = unit(c(0.28, 0.1, 0, 0), "cm"),
        legend.key.size = unit(0.25,"line"),
        legend.margin=margin(unit(c(-4,0,-9,0),"cm")),
        legend.text = element_text(margin = margin(l=-0.065,r = 0.155, unit = "cm")),
        legend.justification = "left",
        plot.title.position = 'plot',
        plot.subtitle = element_text(face = "bold"))+
  guides(fill = guide_legend(reverse = TRUE))



breaks = c(as.Date("2013-01-01"),
           as.Date("2014-01-01"),
           as.Date("2015-01-01"),
           as.Date("2016-01-01"),
           as.Date("2017-01-01"),
           as.Date("2018-01-01"),
           as.Date("2019-01-01"),
           as.Date("2020-01-01"),
           as.Date("2021-01-01"))

ybreaks = c(0,2,4,6,8,10,12,14,16)

#Construct time series plot on the left
q<-ggplot(data=historic, aes(x=Date, y=FixedRatio, group=City)) +
  geom_line(aes(color=City))+
  #Very fiddly manual label placement
  geom_label(aes(x=as.Date("2012-09-01"),y=16.8,label="By year and UK city"),colour='black',
             fill='white', size=1.4,label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  geom_label(aes(x=as.Date("2013-01-01"),y=15.2,label="Cambridge"),colour='black',
             fill='white', size=labsize,label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  geom_label(aes(x=as.Date("2013-01-01"),y=6.1,label="Leicester"),colour='black',
             fill='white', size=labsize, label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  geom_label(aes(x=as.Date("2013-01-01"),y=1,label="Liverpool"),colour='black',
             fill='white', size=labsize, label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  geom_label(aes(x=as.Date("2017-06-01"),y=5.7,label="London"),colour='black',
             fill='white', size=labsize, label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  geom_label(aes(x=as.Date("2015-04-01"),y=2.8,label="Birmingham"),colour='black',
             fill='white', size=labsize, label.size = NA,label.padding = labpad,hjust=0,fontface = "bold")+
  
  theme_minimal()+
  ylab(element_blank())+
  xlab(element_blank())+
  scale_y_continuous(expand=c(0,0),breaks=ybreaks,position = "right",limits = c(0,17.2))+
  scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = breaks,labels=c(2013:2021))+
  scale_colour_manual(values=c('#00ffcc','#00e8ff','#00c3ff','#a685ff','#ff10aa'))+
  labs(title="Going Dutch",
       subtitle="Proportion of roads and paths with cycling infrastructure, %")+
  theme(legend.position='none',
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(size=0.2),
        text = element_text(size = 4,colour='black'),
        axis.text.x = element_text(colour='black',margin = margin(t=0, unit = "mm")),
        axis.text.y = element_text(colour='black'),
        plot.title = element_text(face = "bold",size=7),
        plot.subtitle = element_text(vjust=6.5,size=4.3,margin = margin(b=-0.13, unit = "cm")),
        plot.margin = unit(c(0.05, 0, 0, 0), "cm"),
        axis.ticks.y = element_blank(),
        axis.ticks.length.y = unit(0, "lines"),
        axis.ticks.x = element_line(size=0.2),
        axis.ticks.length.x = unit(0.1, "lines"))

#Add source details
q<-add_sub(q, "Source: OpenStreetMap", y  = 0.1,x=0, vjust = -0.5,hjust=0,size=3,vpadding=grid::unit(0, "lines"))

#Add caveat about Paris Data
p<-add_sub(p, "*includes the departments Hauts-de-Seine, Seine-Saint-Denis and Val-de-Marne",
           x=1, y=0.1, vjust = -0.5,hjust=1,size=3,vpadding=grid::unit(0, "lines"))

#Combine plots and save
tog <- plot_grid(q,p)

wd <- getwd()
dest <- paste0(wd, '/CycleLanes/CycleChart.png')

save_plot(dest,tog,base_height = 2.22,base_width = 3.3334,units = 'in')
