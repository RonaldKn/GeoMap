AddressToLongLat <- function(map, file){
  
  # code desinged by Ronald C. Knetsch
  # csv file should consist of 4 columns with address information (eg.: street, streetNr, zip, city)
  # and should use a ';' separator
  
  # map = input and output directory (eg.: "c:\temp\")
  # file = name of input file (eg.: "address.csv")
  # erea = erae for geo map (eg.: "Nederland")
  # zoom = level of detail, 1 - 21 (whole numbers)
  
  # use package ggmap
  # for more info see: http://gis.stackexchange.com/questions/68663/population-density-map
  #install.packages("ggmap")
  library(ggmap)
  
  # read adress file
  df.address <- read.csv2(paste(map, file, sep=""))
  df.address$Street_cor <- NA
  df.address$Long <- NA
  df.address$Lat <- NA
  
  i <- 0
  n <- 1
  while(i < nrow(df.address)){
    df.address[n,5] <- gsub("\\ *[0-9]*-$", "", df.address[n,1])
    df.address[n,5] <- gsub("\\ *[0-9]*$", "", df.address[n,5])
    location <- paste(df.address[n,5], df.address[n,2],
                      df.address[n,3], df.address[n,4])
    long_lat <- geocode(location,
                        output = c("latlon", "latlona", "more", "all"),
                        messaging = FALSE, sensor = FALSE,
                        override_limit = FALSE)
    df.address[n,6] <- long_lat[1]
    df.address[n,7] <- long_lat[2]
    print(n)
    i <- i + 1
    n <- n + 1
  }

  print(head(df.address, 20))
  write.csv2(df.address, paste(map, "AddressToLongLat.csv", sep=""), row.names=FALSE, na="")
}