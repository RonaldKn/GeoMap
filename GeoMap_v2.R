geomap <- function(map, file, area="Nederland", zoom=7, type="terrain"){
  
  # GENERAL INFORMATION
  # code desinged by Ronald C. Knetsch (mail@ronaldknetsch.nl)
  # csv may consist of multiple columns, a column for longitude and latitude must be included
  # csv column separator should be ';'
  # csv column for segmentation group should be 'GroupID' (eg. min 1, max 3)
  # csv column for longitude should be 'Long'
  # csv column for latitude should be 'Lat'
  # csv column for segmentation color should be 'Color'
  # csv column for segmentation size should be 'Size'
  # csv column for segmentation transparency should be 'Alpha' (eg. min .1, max 1)
  
  # FUNCTION PARAMETERS
  # map = input and output directory (eg.: "c:\temp\")
  # file = name of input file (eg.: "locations.csv")
  # area = erae for geo map (eg.: "Nederland")
  # zoom = level of detail, 1 - 21 (whole numbers)
  # type = Google map type (eg.:"terrain", "roadmap", "satelite", "hybride")
  
  # use package ggmap
  # for more info see: http://gis.stackexchange.com/questions/68663/population-density-map
  #install.packages("ggmap")
  library(ggmap)
  
  # read adress file
  df.points <- read.csv2(paste(map, file, sep=""))
  
  
  # get map and plot data on 
  #map <- get_map(location=area, zoom=zoom, maptype=type)
  map <- get_map(location=area, zoom=zoom, maptype=type, color = "bw")

  
  # subset segmentation groups
  # count groups
  df.points$GroupID <-as.factor(df.points$GroupID)
  #i <- nlevels(df.points$GroupID)
  i <- 3L
  if(nlevels(df.points$GroupID) == 1){
    df.group1 <- subset(df.points, df.points$GroupID == 1)
    color1 <- as.character(df.group1$Color[1])
    size1 <- as.numeric(df.group1$Size[1])
    alpha1 <- as.numeric(df.group1$Alpha[1])
    
    
    ggmap(map) + geom_point(aes(x=Long, y=Lat), data=df.group1, color=color1, size=size1, alpha=alpha1)
  }else if(nlevels(df.points$GroupID) == 2){
    df.group1 <- subset(df.points, df.points$GroupID == 1)
    df.group2 <- subset(df.points, df.points$GroupID == 2)
    color1 <- as.character(df.group1$Color[1])
    color2 <- as.character(df.group2$Color[1])
    size1 <- as.numeric(df.group1$Size[1])
    size2 <- as.numeric(df.group2$Size[1])
    alpha1 <- as.numeric(df.group1$Alpha[1])
    alpha2 <- as.numeric(df.group2$Alpha[1])
    
    
    ggmap(map) + geom_point(aes(x=Long, y=Lat), data=df.group1, color=color1, size=size1, alpha=alpha1) +
      geom_point(aes(x=Long, y=Lat), data=df.group2, color=color2, size=size2, alpha=alpha2)
  }else if(nlevels(df.points$GroupID) == 3){
    df.group1 <- subset(df.points, df.points$GroupID == 1)
    df.group2 <- subset(df.points, df.points$GroupID == 2)
    df.group3 <- subset(df.points, df.points$GroupID == 3)
    color1 <- as.character(df.group1$Color[1])
    color2 <- as.character(df.group2$Color[1])
    color3 <- as.character(df.group3$Color[1])
    size1 <- as.numeric(df.group1$Size[1])
    size2 <- as.numeric(df.group2$Size[1])
    size3 <- as.numeric(df.group3$Size[1])
    alpha1 <- as.numeric(df.group1$Alpha[1])
    alpha2 <- as.numeric(df.group2$Alpha[1])
    alpha3 <- as.numeric(df.group3$Alpha[1])
    
    
    #ggmap(map) + geom_point(aes(x=Long, y=Lat), data=df.group1, color=color1, size=size1, alpha=alpha1) +
    #  geom_point(aes(x=Long, y=Lat), data=df.group2, color=color2, size=size2, alpha=alpha2) +
    #  geom_point(aes(x=Long, y=Lat), data=df.group3, color=color3, size=size3, alpha=alpha3)
    
    ggmap(map, extent = "normal", maprange=FALSE) %+% df.points + aes(x = Long, y = Lat) +
      geom_density2d() +
      stat_density2d(aes(fill = ..level.., alpha = ..level..),
                     size = 0.01, bins = 16, geom = 'polygon') +
      scale_fill_gradient(low = "yellow", high = "red") +
      scale_alpha(range = c(0.1, 0.4), guide = FALSE) +
      coord_map(projection="mercator",
                xlim=c(attr(map, "bb")$ll.lon, attr(map, "bb")$ur.lon),
                ylim=c(attr(map, "bb")$ll.lat, attr(map, "bb")$ur.lat)) +
      theme(legend.position = "none", axis.title = element_blank(), text = element_text(size = 12)) +
      geom_point(aes(x=Long, y=Lat), data=df.group2, color=color2, size=size2, alpha=alpha2) +
      geom_point(aes(x=Long, y=Lat), data=df.group3, color=color3, size=size3, alpha=alpha3)
  }else{
    print("Too many segmentation groups. Max. 3 groups allowed")
  }
  
}