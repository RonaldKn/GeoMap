GeoMap <- function(map, file, erea="Nederland", zoom=7, type="terrain",
                   size=3, alpha=.5, color="darkred"){
  
  # GENERAL INFORMATION
  # code desinged by Ronald C. Knetsch (mail@ronaldknetsch.nl)
  # csv may consist of multiple columns, a column for longitude and latitude must be included
  # csv column separator should be ';'
  # csv column for longitude should be 'Long'
  # csv column for latitude should be 'Lat'
  
  # FUNCTION PARAMETERS
  # map = input and output directory (eg.: "c:\temp\")
  # file = name of input file (eg.: "address.csv")
  # erea = erae for geo map (eg.: "Nederland")
  # zoom = level of detail, 1 - 21 (whole numbers)
  # type = Google map type (eg.:"terrain", "roadmap", "satelite", "hybride")
  # size = size of points in plot
  # alpha = transparency of points in plot
  # color = color of points in plot
  
  # use package ggmap
  # for more info see: http://gis.stackexchange.com/questions/68663/population-density-map
  #install.packages("ggmap")
  library(ggmap)
  
  # read adress file
  df.geomap <- read.csv2(paste(map, file, sep=""))
  
  # get map and plot data on 
  map <- get_map(location=erea, zoom=zoom, maptype=type)
  ggmap(map)
    + geom_point(aes(x=Long, y=Lat), data=df.geomap, alpha=alpha, color=color, size=size)
}