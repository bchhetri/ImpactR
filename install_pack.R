#SCRIPT TO INSTALL NECESSARY R PACKAGES FOR ImpactR

install.packages("devtools")
require(devtools)
install_github('ramnathv/rCharts')
install.packages("plyr","dplyr","lubridate","shiny","ggplot2","stringr","leaflet","reshape2")
install.packages("scales","grid","DT","dlnm","splines","ncdf")
