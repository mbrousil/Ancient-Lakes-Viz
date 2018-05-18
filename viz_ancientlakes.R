# in a row, should all have same scale bar
# do this when get rest figured out
# make any background match color of lakes

######### load packages ############
library(grid)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(ggmap)
library(rgdal)
library(raster)

#for scale bar
library(maps)
library(maptools)
library(ggplot2)
library(grid)
# http://egallic.fr/scale-bar-and-north-arrow-on-a-ggplot2-map/
# devtools::install_github("3wen/legendMap")
library(legendMap)

###############################################################
################## Load and process data ######################
###############################################################

# read in data
Data.orig <-read.csv("Lake_phys_latlong.csv", stringsAsFactors = FALSE)
data<-Data.orig
data<-data[order(data$Form,data$Lake.Name),]
data$lakelabel<-paste(data$Lake.Name, " (", data$Form, ")", sep="")
data$lakelabel <- gsub("Lake ", "", data$lakelabel)
data$lakelabel2 <- data$Code

#make lake factor
data$fact <- factor(data$lakelabel2, 
                         levels = c("ELGY", "PING", "POTR", "TULE", "TAHO", 
                                    "PRES", "OHRI", "KINN", "POSO",
                                    "BOSU",
                                    "INLE", "LANA", "MATA",
                                    "VALE", "BIWA", 
                                    "MANI", "HOVS", "ISSY",
                                    "VANL", "ZAYS",
                                    "EYRE", "MARA", "TITI", 
                                    "BAIK", "ARAL", "VICT",
                                    "MALA", "TANG", "CASP"))

data <- arrange(data, fact)

data$index<-1:length(data[,2])

data$lakelabel <- gsub("Lake ", "", data$lakelabel)
data$lakelabel <- gsub(" Sea", "", data$lakelabel)
data$lakelabel <- gsub(" Reservoir", "", data$lakelabel)
data$lakelabel <- gsub("(Khuvsgul)", "", data$lakelabel)
data$lakelabel <- gsub("(Nyasa, Niassa)", "", data$lakelabel)
data$lakelabel <- gsub("(Sea of Galilee)", "", data$lakelabel)
data$lakelabel <- gsub(" \\()", "", data$lakelabel)
data$lakelabel <- gsub("\\()", "", data$lakelabel)

#make lat/long bounding box for each lake
data1<-data
data2<-data
data2$Latitude<-data2$Latitude-1
data2$Longitude<-data2$Longitude-1
data3<-data
data3$Latitude<-data3$Latitude+1
data3$Longitude<-data3$Longitude-1
data4<-data
data4$Latitude<-data4$Latitude-1
data4$Longitude<-data4$Longitude+1
data5<-data
data5$Latitude<-data5$Latitude+1
data5$Longitude<-data5$Longitude+1
data<-rbind(data1,data2,data3,data4,data5)
#data<-data[order(data$Form,data$Lake.Name),]

#Global Wetland Lakes Database + HydroLAKES (from SP)
#lakes <-readOGR("Lakes_shapefiles","glwd_use")
lakes <-readOGR(dsn=getwd(),layer="glwd_use")

# lakes_select <- subset(lakes, Lake_name %in% c("El'gygytgyn",
#                                                 "Kinneret", "Inle",
#                                                "Lanao", "Maracaibo"))
# 
# 
# subset(lakes, grepl("*gygy*", Lake_name)) #nope
# subset(lakes, grepl("*Kinner*", Lake_name))
# subset(lakes, grepl("*Lanao*", Lake_name))
# subset(lakes, grepl("*Inle*", Lake_name)) %>% head() #not the one we want
# subset(lakes, grepl("*Marac*", Lake_name)) %>% head() #nope


have <- c("Aral Sea", "Baikal", "Biwa-ko", "Bosumtwi", "Eyre", 
          "Lake Tahoe", "Malawi", "Manicouagan", "Hovs Gol",
"Ohrid", "Pingualuit", "Prespa", "Tahoe", "Tanganyika", 
"Issyk-kul", "Danau Matano", "Danau Poso",
"Titicaca", "Tule", "Valencia", "Van", "Lake Victoria", 
"Zaysan", "Potrok-Aike", "Caspian Sea", 
"Tiberias", "Lano", "Eloygytgyn")

lakes_select <- subset(lakes, Lake_name %in% have)

lakes_inle <- subset(lakes, GLWD_ID == 3066) #Inle
lakes_maracaibo <- subset(lakes, GLWD_ID == 0 & Hylak_id == 0) #Maracaibo

#stack together
lakes_select2 <- rbind(lakes_select, lakes_inle, lakes_maracaibo)

lakes <- spTransform(lakes, CRS("+proj=longlat +datum=WGS84"))
lakes <- fortify(lakes)
lakes <- lakes

lakes_select2 <- spTransform(lakes_select2, CRS("+proj=longlat +datum=WGS84"))
lakes_select2 <- fortify(lakes_select2)
lakes_select2 <- lakes_select2


islands<-subset(lakes,lakes$hole==TRUE)

# mp <- NULL
mapWorld <- borders("world", colour="gray95", fill="gray95") 

#base map
mp <- ggplot() +   mapWorld #+ theme_opts

#adding GLWD lakes to world map
mp <- mp+ geom_polygon(aes(x=long, y=lat, group=group), fill='slategray3',color='slategray3', data=lakes)

#lakes of interest
mp <- mp+ geom_polygon(aes(x=long, y=lat, group=group), fill='cornflowerblue',color='cornflowerblue', data=lakes_select2)

#adding islands
mp <- mp+ geom_polygon(aes(x=long, y=lat, group=group), fill='gray95',color='gray95', data=islands)

#expanding bounding boxes for big lakes
data$Longitude[which(data$Lake.Name=="Manicouagan Reservoir")]<-
  -0.4+data$Longitude[which(data$Lake.Name=="Manicouagan Reservoir")]
data$Longitude[which(data$Lake.Name=="Aral Sea")]<-
  0.9+data$Longitude[which(data$Lake.Name=="Aral Sea")]
data$Latitude[which(data$Lake.Name=="Lake Zaysan")]<-
  0.35+data$Latitude[which(data$Lake.Name=="Lake Zaysan")]
data$Latitude[which(data$Lake.Name=="Lake Biwa")]<-
  -0.15+data$Latitude[which(data$Lake.Name=="Lake Biwa")]
data$Longitude[which(data$Lake.Name=="Lake Baikal")]<-
  -1.0+data$Longitude[which(data$Lake.Name=="Lake Baikal")]

#contracting bounding boxes for small lakes
data$Longitude[which(data$Lake.Name=="Lake Pingualuit")] <- (data$Longitude[which(data$Lake.Name=="Lake Pingualuit")]+data$Longitude[which(data$Lake.Name=="Lake Pingualuit")][1])/2
data$Latitude[which(data$Lake.Name=="Lake Pingualuit")] <- (data$Latitude[which(data$Lake.Name=="Lake Pingualuit")]+data$Latitude[which(data$Lake.Name=="Lake Pingualuit")][1])/2

data$Longitude[which(data$Lake.Name=="Lake Potrok-Aike")] <- (data$Longitude[which(data$Lake.Name=="Lake Potrok-Aike")]+data$Longitude[which(data$Lake.Name=="Lake Potrok-Aike")][1])/2
data$Latitude[which(data$Lake.Name=="Lake Potrok-Aike")] <- (data$Latitude[which(data$Lake.Name=="Lake Potrok-Aike")]+data$Latitude[which(data$Lake.Name=="Lake Potrok-Aike")][1])/2

data$Longitude[which(data$Lake.Name=="Lake Tule")] <- (data$Longitude[which(data$Lake.Name=="Lake Tule")]+data$Longitude[which(data$Lake.Name=="Lake Tule")][1])/2
data$Latitude[which(data$Lake.Name=="Lake Tule")] <- (data$Latitude[which(data$Lake.Name=="Lake Tule")]+data$Latitude[which(data$Lake.Name=="Lake Tule")][1])/2

data$Longitude[which(data$Lake.Name=="Lake Bosumtwi")] <- (data$Longitude[which(data$Lake.Name=="Lake Bosumtwi")]+data$Longitude[which(data$Lake.Name=="Lake Bosumtwi")][1])/2
data$Latitude[which(data$Lake.Name=="Lake Bosumtwi")] <- (data$Latitude[which(data$Lake.Name=="Lake Bosumtwi")]+data$Latitude[which(data$Lake.Name=="Lake Bosumtwi")][1])/2

data$Longitude[which(data$Lake.Name=="Lake Inle")] <- (data$Longitude[which(data$Lake.Name=="Lake Inle")]+data$Longitude[which(data$Lake.Name=="Lake Inle")][1])/2
data$Latitude[which(data$Lake.Name=="Lake Inle")] <- (data$Latitude[which(data$Lake.Name=="Lake Inle")]+data$Latitude[which(data$Lake.Name=="Lake Inle")][1])/2


#classify lakes

reg_lakes_12 <- c("BOSU", "ELGY", "PING",
              "INLE", "KINN", "LANA", "MATA",
              "OHRI", "POSO", "PRES", "TAHO", 
              "TULE", "VALE", "POTR")

biwa <- c("BIWA")

larger_lakes_50 <- c("MANI", "HOVS", "ISSY", "VANL", "ZAYS")

larger_lakes_100 <- c("TITI", "MARA", "EYRE")

larger_lakes_100_2 <- c("BAIK", "ARAL")

huge_lakes_200 <- c("VICT", "MALA", "TANG")

caspian <- c("CASP")

#updated scale_bar function
scale_barCoug<-function (lon, lat, distance_lon, distance_lat, distance_legend, 
                         dist_unit = "km", rec_fill = "black", rec_colour = "black", 
                         rec2_fill = "black", rec2_colour = "black", legend_colour = "black", alphas=c(0,0,1),
                         legend_size = 3, orientation = TRUE, arrow_length = 500, 
                         arrow_distance = 300, arrow_north_size = 6) 
{
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, 
                                    distance_lat = distance_lat, distance_legend = distance_legend, 
                                    dist_unit = dist_unit)
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, 
                             aes(x = lon, y = lat), fill = rec_fill, colour = rec_colour)
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, 
                             aes(x = lon, y = lat), fill = rec2_fill, colour = rec2_colour)
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[, 
                                                                          "text"], dist_unit, sep = ""), x = the_scale_bar$legend[, 
                                                                                                                                  "long"], y = the_scale_bar$legend[, "lat"], size = legend_size, 
                               colour = legend_colour,alpha=alphas)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  if (orientation) {
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, 
                                             length = arrow_length, distance = arrow_distance, 
                                             dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, 
                                                            y = y, xend = xend, yend = yend)), annotate("text", 
                                                                                                        label = "N", x = coords_arrow$coords_n[1, "x"], y = coords_arrow$coords_n[1, 
                                                                                                                                                                                  "y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}


#loop to go through each and subset to bounding box
for(i in 1:29){

datai <- filter(data, index == i)

#this plot an invisible bounding box by points (?)
mpi<-mp+geom_point(aes(x = Longitude, y = Latitude), data = datai, alpha = 0.0,
                   color = "dark green")

xlims<-c(min(datai$Longitude-((datai$size*1.6)-2.4)),max(datai$Longitude+((datai$size*1.6)-2.4)))
ylims<-c(min(datai$Latitude-((datai$size*1.6)-2.4)),max(datai$Latitude+((datai$size*1.6)-2.4)))

loni <- xlims[1]
lati <- ylims[1]

if (unique(datai$Code) %in% reg_lakes_12) {
  dist_loni <- 7.5
  dist_lati <- 0.25
  dist_i <- 4
}

if (unique(datai$Code) %in% biwa) {
  dist_loni <- 7.5
  dist_lati <- 0.25
  dist_i <- 6
}

if(unique(datai$Code) %in% larger_lakes_50){
  dist_loni <- 25
  dist_lati <- 1
  dist_i <- 20
}

if(unique(datai$Code) %in% larger_lakes_100){
  dist_loni <- 75
  dist_lati <- 1.8
  dist_i <- 30
}

if(unique(datai$Code) %in% larger_lakes_100_2){
  dist_loni <- 75
  dist_lati <- 1.8
  dist_i <- 70
}

if(unique(datai$Code) %in% huge_lakes_200){
  dist_loni <- 100
  dist_lati <- 1.8
  dist_i <- 70
}

if(unique(datai$Code) %in% caspian){
  dist_loni <- 100
  dist_lati <- 1.8
  dist_i <- 130
}


#zoom in
mpi<-mpi+ coord_equal(xlim=xlims, ylim=ylims)+
  theme(legend.position="none") +
   theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank(),
         strip.text.x = element_text(size = 11.5),
         plot.margin = unit(c(0.15, 0.15, 0.15, 0.15), "cm"),
         panel.background = element_rect(fill = 'slategray3', colour = 'slategray3'),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank()) +
  scale_barCoug(lon = loni, lat = lati,
            distance_lon = dist_loni, distance_lat = dist_lati, distance_legend = dist_i,
            dist_unit = "km", orientation = FALSE, legend_size = 4) +
  facet_wrap(~lakelabel)

assign(paste("plot",i,sep=""),mpi)
}

#arrange all plots
png(filename="lake_shapes_figure_test.png",height=10,width=11,units="in",res=600)
grid.arrange(plot1,plot2,plot3,plot4,plot5,
             plot6,plot7,plot8,plot9,plot10,
             plot11,plot12,plot13,plot14,plot15,
             plot16,plot17,plot18,plot19,plot20,
             plot21,plot22,plot23,plot24,plot25,
             plot26,plot27,plot28,plot29,
             ncol=6,widths=c(1.5,1.5,1.5,1.5,1.5,1.5))
dev.off()
