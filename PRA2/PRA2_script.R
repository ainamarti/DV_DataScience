## PRA 1 data visualization

setwd("/Users/Aina/Library/CloudStorage/GoogleDrive-amartiaran@uoc.edu/La meva unitat/04-semestre3/data_visualization/PRA2")
#setwd("/Users/Aina/Library/CloudStorage/GoogleDrive-amartiaran@uoc.edu/La meva unitat/04-semestre3/data_visualization/PRA1")

universities<-read.csv("2023University_Rankings.csv")
str(universities)

library(ggplot2)
library(dplyr) 
library(data.table)
universities<-as.data.table(universities)
universities_<-as.data.table(universities %>% group_by(location) %>% dplyr::summarise(mean_rank=as.numeric(mean(Rank)), max_rank=as.numeric(min(Rank))))

uni_melt <- melt(universities_, id.vars = 'location', variable.name = 'rank_type', 
                       value.name = 'ranking')


#mean and max scatterplot
ggplot(data=uni_melt) +
  geom_point(aes(reorder(location, ranking, mean), y=ranking, color=rank_type), size=2, alpha=0.9) + 
  theme_light()+
  theme(axis.text.x=element_text(angle=90, size=13), 
        axis.text.y=element_text(size=15), 
        axis.title.y = element_blank(),
        legend.title = element_blank()) + 
  scale_y_reverse() + 
  xlab("") + 
  ylab("Rankings") + 
  scale_color_manual(values=c("deeppink", "black"))+
  ggtitle("World wide university rankings")

ggsave("rankings_all.png", width=18, height=5)



# TOP 20 universities tables
library(tidyverse)
world_coordinates <- map_data("world")
universities_10<-universities[Rank<=10]
library(gridExtra)
t1 <- ttheme_minimal(core=list(
  fg_params=list(fontface=c(rep("plain", 4))),
  bg_params = list(fill=c(rep(c("white", "grey90"),
                              length.out=4)))),
  colhead=list(fg_params=list(col="deeppink4")
))
grid.table(universities_10[,c(1, 2, 4)], , theme = t1) 


universities_10<-universities[Rank>10 & Rank<=20]
library(gridExtra)
t1 <- ttheme_minimal(core=list(
  fg_params=list(fontface=c(rep("plain", 4))),
  bg_params = list(fill=c(rep(c("white", "grey90"),
                              length.out=4)))),
  colhead=list(fg_params=list(col="deeppink4")
  ))
grid.table(universities_10[,c(1, 2, 4)], , theme = t1) 




#global map
library(tidygeocoder)
lat_longs <- as.data.frame(universities_2) %>%
  geocode(location, method = 'osm', lat = latitude , long = longitude)
colnames(world_coordinates)[5]<-"location"

world_coordinates$location
world_coordinates

universities_<-as.data.table(universities %>% group_by(location) %>% dplyr::summarise(mean_rank=as.numeric(mean(Rank)), max_rank=as.numeric(min(Rank))))

universities_[location=="United Kingdom"]$location<-"UK"
universities_[location=="United States"]$location<-"USA"
universities_[location=="China (Mainland)"]$location<-"China"

world_coordinates_<-left_join(world_coordinates, universities_,   by="location")

library(ggiraph)
ggplot(world_coordinates_, aes(x=long, y=lat)) + 
  geom_polygon_interactive(aes(group=group, fill=mean_rank, tooltip = group, data_id = group), col=NA,lwd=0)+

  theme_light()+
  theme(axis.text.y=element_blank(), 
            axis.text.x=element_blank(), 
            axis.title.y = element_blank(),
          axis.title.x = element_blank(),
            legend.title = element_blank()) + 
  scale_fill_gradient(low = "deeppink4", high = "white", na.value = "grey")

ggsave("rankings_map_mean.png", width=17, height=10)


world_coordinates__<-world_coordinates_
world_coordinates__$max_rank<-as.numeric(world_coordinates__$max_rank)
world_coordinates__[max_rank>500]$max_rank<-500



# TOP 25 universities
library(tidyverse)
#library(RColorBrewer)
#library(pals)

universities_100<-universities[Rank<=25]
universities_<-as.data.table(universities_100 %>% group_by(location) %>% dplyr::summarise(count=n()))
hsize <- 2

best_universities<-unique(universities_$location)

universities_ <- universities_ %>%
  mutate(x = hsize)

ggplot(universities_, aes(x = hsize, y = count, fill = location)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") + theme_void()+
  geom_text(aes(label = count),
            position = position_stack(vjust = 0.5), size=7) +
  xlim(c(0.2, hsize + 0.5)) + 
  scale_fill_brewer(palette="RdPu") 

ggsave("rankings_25.png", width=5, height=4)



# top 10 countroes with best mean ranking
universities_2<-as.data.table(universities %>% group_by(location) %>% dplyr::summarise(mean_rank=mean(Rank), count=n()))
universities_2<-universities_2[order(mean_rank)][1:10]
hsize <- 2
best_mean_countries<-universities_2$location



universities_2 <- universities_2 %>%
  mutate(x = hsize)


ggplot(universities_2)+
  geom_bar(stat="identity", aes(x=location, y=mean_rank, fill=mean_rank))+
  #scale_fill_continuous(palette="RdPu")+
  theme_void()+
  theme(axis.text.x=element_text(angle=90, size=10), axis.text.y=element_text( size=10), legend.position = "none")+
scale_fill_continuous(low="deeppink4", high="lavenderblush") + ylab("mean ranking")

ggsave("rankings_10.png", width=5, height=4)




#violin plots
universities<-as.data.table(universities)
universities_<-universities[location %in% c(best_mean_countries, best_universities)]


ggplot(universities_, aes(x=location, y=Rank))+
  geom_violin(color="black") +
  geom_point(aes(color=Rank), alpha=0.7)+
  theme_light(base_size=16)+
  scale_color_gradient(low="deeppink4",
                        high="lavenderblush" ) + 
  theme(axis.text.x=element_text(angle=90)) + ylim(0, max(universities$Rank))

ggsave("ranking_violins.png", width=10, height=7)















setwd("/Users/Aina/Library/CloudStorage/GoogleDrive-amartiaran@uoc.edu/La meva unitat/04-semestre3/data_visualization/PRA2")
universities<-read.csv("2023University_Rankings.csv")
universities<-as.data.table(universities)
universities<-universities[,c(1, 2, 4, 6, 8, 10, 12, 14, 18, 20)]
colnames(universities)<-c("Rank", "institution", "location", "Academic Reputation Rank",  "Employee reputation rank","Student Rank",
                          "Citations Rank", "International Faculty rank", "International research network rank", "Employment Outcome rank")

universities_mean<-as.data.table(universities %>% group_by(location) %>% dplyr::summarise(mean_rank=as.numeric(mean(Rank)), max_rank=as.numeric(min(Rank))))

universities_mean[location=="United Kingdom"]$location<-"UK"
universities_mean[location=="United States"]$location<-"USA"
universities_mean[location=="China (Mainland)"]$location<-"China"

world_coordinates <- map_data("world")
colnames(world_coordinates)[5]<-"location"
world_coordinates_meanRank<-left_join(world_coordinates, universities_mean,   by="location")
write.csv(world_coordinates_meanRank, "world_coordinates_universitiesMeans.csv")

universities_coordinates<-as.data.frame(universities %>%
                                          geocode(institution, method = 'osm', lat = latitude , long = longitude))



write.csv(universities_coordinates, "universities_coordinates.csv")

