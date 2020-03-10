
library(tidyverse)
library(haven)

fp <- haven::read_stata("./data/Jordan/TZAPS17_fp.dta")
area <- fp %>% select(matches("area"))
area

area_est	What is the farmers estimate of the area of this plot?
area_gps_note	Please use the UTM Area Measure app to measure the boundary of the plot.
area_gps	What is the GPS reading of the area of this plot?
area_gps_calc	
area_gps_conv	The area you entered is equivalent to ${area_gps_calc} acres. If this value seems incorrect, please go back and check your entry on previous screen.
area_gps_conf	Did you save the polygon boundary in your device?
