geomap <- function(map, file1, file2="none", area="Nederland", zoom=7, type="terrain",
                   size=3, alpha=.5, color1="darkred", color2="purple"){
  
  # GENERAL INFORMATION
  # code desinged by Ronald C. Knetsch (mail@ronaldknetsch.nl)
  # csv may consist of multiple columns, a column for longitude and latitude must be included
  # csv column separator should be ';'
  # csv column for longitude should be 'Long'
  # csv column for latitude should be 'Lat'
  
  # FUNCTION PARAMETERS
  # map = input and output directory (eg.: "c:\temp\")
  # file1 = name of input file for general points (eg.: "locations.csv")
  # file2 = name of input file for refference points (eg.: "ref_locations.csv")
  # area = erae for geo map (eg.: "Nederland")
  # zoom = level of detail, 1 - 21 (whole numbers)
  # type = Google map type (eg.:"terrain", "roadmap", "satelite", "hybride")
  # size = size of points in plot
  # alpha = transparency of points in plot
  # color1 = color for general points in plot
  # color2 = color for reference points in plot
  
  # use package ggmap
  # for more info see: http://gis.stackexchange.com/questions/68663/population-density-map
  #install.packages("ggmap")
  library(ggmap)
  
  # read adress file
  if(file2=="none"){
    df.general <- read.csv2(paste(map, file1, sep=""))
  }else{
    df.general <- read.csv2(paste(map, file1, sep=""))
    df.refference <- read.csv2(paste(map, file2, sep=""))
  }
  
  
  # get map and plot data on 
  map <- get_map(location=area, zoom=zoom, maptype=type)
  
  if(file2=="none"){
    ggmap(map) + geom_point(aes(x=Long, y=Lat), data=df.general, alpha=alpha, color=color1, size=size)
  }else{
    ggmap(map) + geom_point(aes(x=Long, y=Lat), data=df.general, alpha=alpha, color=color1, size=size) + geom_point(aes(x=Long, y=Lat), data=df.refference, alpha=.8, color=color2, size=6) 
  }
}