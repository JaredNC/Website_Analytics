library(iptools)
library(rgeolocate)
library(tidyverse)

ips <- post[1:10000,3]

system.time(
  rgeolocate::maxmind(
    ips, "~\GeoLite2-City_20181016\GeoLite2-City_20181016/GeoLite2-City.mmdb", c("longitude", "latitude")
  ) -> xdf
)

