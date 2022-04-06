# Authors: Mike Ackerman, Kevin See, and Richie Carmichael
#
# Purpose: Identify habitat reaches with missing data, and help determine why the NAs exist
#
# Created: August 19, 2020
#
# Notes: In the Pahsimeroi, habitat reach 5, channel units 27:30, a large side channel, missing lots of data. Also, appears to have 2 centerlines, causing issues with sinuousity and braidedness metrics

#-----------------------------
# load necessary libraries
#-----------------------------
library(sf)
library(tidyverse)
library(magrittr)

#-------------------------
# set NAS prefix, depending on operating system
#-------------------------
if(.Platform$OS.type != 'unix') {
  nas_prefix = "S:"
}
if(.Platform$OS.type == 'unix') {
  nas_prefix = "~/../../Volumes/ABS/"
}

#-------------------------------------
# load prepped data
load(paste0(nas_prefix, "data/habitat/DASH/prepped/2018/dash2018_cus.Rda"))
load(paste0(nas_prefix, "/data/habitat/DASH/prepped/2018/dash2018_hr.Rda"))

# which metrics have some NA's?
names(dash2018_hr)[sort(unique(which(is.na(dash2018_hr), T)[,'col']))]

# drop Pahsimeroi reach 5, it has other issues
dash2018_hr_v2 = dash2018_hr %>%
  filter(!(grepl("Pah", SiteNam) & Hab_Rch == 5))

# now which metrics have some NA's?
names(dash2018_hr_v2)[sort(unique(which(is.na(dash2018_hr_v2), T)[,'col']))]

#-------------------------------------
# missing pool metrics
dash2018_hr %>%
  st_drop_geometry() %>%
  filter(is.na(Pool_Mx_Dpth)) %>%
  select(SiteNam, Hab_Rch, Pool_Cnt) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry()) %>%
  select(SiteNam, Hab_Rch,
         Sgmnt_ID,
         Pool_Cnt,
         CU_ID,
         CU_Typ,
         Mx_Dpth)
# none of these habitat reaches have a pool channel unit. Should max depth be 0 or NA?

# missing D50 or D84
dash2018_hr %>%
  st_drop_geometry() %>%
  filter(is.na(d50) |
           is.na(d84)) %>%
  select(SiteNam, Hab_Rch, Riffle_Cnt) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry()) %>%
  select(SiteNam, Hab_Rch,
         Sgmnt_ID,
         Riffle_Cnt,
         CU_ID,
         CU_Typ,
         d50, d84)
# none of these habitat reaches have a riffle channel unit, so no pebble counts.

# missing percent substrates
dash2018_hr %>%
  st_drop_geometry() %>%
  filter(is.na(Prc_Fns)) %>%
  select(SiteNam, Hab_Rch) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry()) %>%
  select(SiteNam, Hab_Rch,
         Sgmnt_ID,
         CU_ID,
         CU_Typ,
         starts_with("Prc"))
# these reaches don't have any pools or runs (which appear to be the only type of channel unit where percent substrate was recorded)

dash2018_cu %>%
  xtabs(~ CU_Typ + is.na(Prc_Fns), .)

dash2018_cu %>%
  st_drop_geometry() %>%
  filter(is.na(Prc_Fns)) %>%
  # xtabs(~ CU_Typ, .)
  select(SiteNam, Hab_Rch,
         Sgmnt_ID,
         CU_ID,
         CU_Typ,
         starts_with("Prc"))


# missing length related metrics
dash2018_hr %>%
  st_drop_geometry() %>%
  filter(is.na(sinuosity) |
           is.na(Length.y) |
           is.na(straight_line) |
           is.na(sc_Length) |
           is.na(Tot_Length) |
           is.na(wet_braid) |
           is.na(wet_braid_sin)) %>%
  select(SiteNam, Hab_Rch,
         Length.x,
         SHAPE_A,
         sinuosity:wet_braid_sin) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry() %>%
              select(SiteNam, Hab_Rch,
                     Sgmnt_ID,
                     CU_ID,
                     CU_Typ,
                     Length,
                     CH_area = SHAPE_A))
# only missing habitat reach is Pahsimeroi reach 9
# Pahsimeroi reach 9 doesn't appear in the centerline shapefile used to calculate sinuosity metrics, but it does appear in the polygon file ph_ms_sf (cu # 73:77). So channel unit metrics exist, but none of the sinuosity / braidedness metrics

#---------------------------------
# OTHER ISSUES
#---------------------------------
# Seven pool channel units have max depth of 0
dash2018_cu %>%
  st_drop_geometry() %>%
  filter(Mx_Dpth == 0) %>%
  select(SiteNam, Hab_Rch,
         CU_ID,
         CU_Typ,
         Glbl_ID,
         Mx_Dpth)

# 2 small side channel have an avg width of 0
dash2018_cu %>%
  st_drop_geometry() %>%
  filter(Avg_Wdth == 0) %>%
  select(SiteNam, Hab_Rch,
         CU_ID,
         CU_Typ,
         Glbl_ID,
         Avg_Wdth)


# Pahsimeroi habitat reach #5
# go get dash2018_raw from dash2018_rollup_v2.R script
dash2018_raw %>%
  filter(grepl("Pah", SiteNam),
         Hab_Roll == 5) %>%
  select(Unt_Typ) %>%
  plot()

dash2018_raw %>%
  filter(grepl("Pah", SiteNam),
         Hab_Roll == 5) %>%
  st_drop_geometry() %>%
  as.data.frame()
# lots of missing metrics in the large side channel. Was it not surveyed on the ground? The only metric present is aquatic veg cover and no cover
# I think these NA's may be causing NAs in the roll-up.

# no wood metrics
dash2018_hr %>%
  filter(grepl("Pah", SiteNam),
         Hab_Rch == 5) %>%
  st_drop_geometry() %>%
  as.data.frame()

# there is a large side channel (channel units 27:31) with no segment ID, and NAs for all the wood metrics
dash2018_cu %>%
  st_drop_geometry() %>%
  filter(grepl("Pah", SiteNam),
         Hab_Rch == 5) %>%
  select(SiteNam, Hab_Rch,
         Sgmnt_ID,
         CU_ID,
         CU_Typ,
         Wod_Cnt:Jam_Vlm)

