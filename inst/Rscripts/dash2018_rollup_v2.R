# Author: Mike Ackerman & Richie Carmichael
#   with assists from Kevin See
# 
# Purpose: Compile the 2018 DASH data from MRA sites to make QRF capacity predictions
#
# Created: Original version on 10/25/2019
# Last Modified: 04/02/2020

# Notes: 

#-----------------------------
# load necessary libraries
#-----------------------------
library(sf)
library(tidyverse)
library(magrittr)

#-----------------------------
# read in 2018 MRA data stored as shapefiles
#-----------------------------
# note we store our 'default' coordinate reference system and set the us crs to be the same as the rest
# EPSG: 32612 = WGS 84/UTM zone 12N; 32611 = WGS 84/UTM zone 11N

# mainstem (ms) channel units, upper lemhi (ul), lower lemhi (ll), pahsimeroi (ph), and upper salmon (us)
ul_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Poly_Fish.shp')
mra_crs = st_crs(ul_ms_sf)
ll_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Poly_Fish.shp')
ph_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Poly_Fish.shp')
us_ms_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Poly_Fish.shp') %>%
  st_transform(crs = mra_crs)

# side channels (sc)
ul_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Line_Fish.shp')
ll_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Line_Fish.shp')
ph_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Line_Fish.shp') 
us_sc_sf = st_read('data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Line_Fish.shp') %>%
  st_transform(crs = mra_crs)

#-----------------------------
# clean and merge data into single sf object
#-----------------------------
dash2018_raw = rbind(ul_ms_sf,
      ll_ms_sf,
      ph_ms_sf,
      us_ms_sf %>%
        rename(Hab_Roll = hr)) %>%
  mutate(Avg_wdt = NA,
         SHAPE_L = NA,
         sinusty = NA) %>%
  rbind(rbind(ul_sc_sf,
              ll_sc_sf,
              ph_sc_sf,
              us_sc_sf %>%
                rename(Hab_Roll = hr) %>%
                mutate(sinusty = NA)) %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA)) %>%
  group_by(SiteNam) %>%
  ungroup()

#-----------------------------
# fix a Lsc in the Pahsimeroi data
dash2018_raw = dash2018_raw %>%
  filter(!(SiteNam == 'Pahsimeroi_2018' & is.na(Hab_Roll))) %>%
  rbind(dash2018_raw %>%
          filter(SiteNam == 'Pahsimeroi_2018' & is.na(Hab_Roll)) %>%
          mutate_at(vars(Off_C_T:sinusty),
                    list(~as.numeric(NA))) %>%
          mutate(Unt_Typ = "Lsc",
                 Sgmnt_N = NA,
                 Hab_Roll = replace_na(Hab_Roll, 5)) )

#-----------------------------
# fix a couple channel units with wrong segment number
# looks like these channel units did not have Survey123 data for some reason

# upper Lemhi unit 151 should be dropped
dash2018_raw %<>%
  filter(!(SiteNam == 'UpperLemhi_2018' & Unt_Nmb == 151))

# fix some data from unit 74 in Pahsimeroi
dash2018_raw <- dash2018_raw %>%
  filter(SiteNam == "Pahsimeroi_2018" & Unt_Nmb == 74) %>%
  mutate(Sgmnt_N = 1,
         Unt_Typ = 'Pool',
         Prc_Fns = 90,
         Prc_Grv = 10,
         Mx_Dpth = 1.32) %>%
  mutate_at(vars(starts_with('Undrc_')),
            list(~ as.numeric(0))) %>%
  rbind(dash2018_raw %>%
          filter(!(SiteNam == "Pahsimeroi_2018" & Unt_Nmb == 74))) %>%
  arrange(SiteNam, Sgmnt_N, Unt_Nmb)

#-----------------------------
# older code
#-----------------------------
# # upper lemhi
# ul_sf = ul_ms_sf %>%
#   mutate(Avg_wdt = NA,
#          SHAPE_L = NA,
#          sinusty = NA) %>%
#   rbind(ul_sc_sf %>%
#           mutate(A_Vg_Cv = NA,
#                  Length = NA))
# #rm(ul_ms_sf, ul_sc_sf)        
# 
# # lower lemhi  
# ll_sf = ll_ms_sf %>%
#   mutate(Avg_wdt = NA,
#          SHAPE_L = NA,
#          sinusty = NA) %>%
#   rbind(ll_sc_sf %>%
#           mutate(A_Vg_Cv = NA,
#                  Length = NA,
#                  Reach = NA) %>%
#           select(-Reach))
# #rm(ll_ms_sf, ll_sc_sf)
# 
# # pahsimeroi
# ph_sf = ph_ms_sf %>%
#   mutate(Avg_wdt = NA,
#          SHAPE_L = NA,
#          sinusty = NA) %>%
#   rbind(ph_sc_sf %>%
#           mutate(A_Vg_Cv = NA,
#                  Length = NA))
# #rm(ph_ms_sf, ph_sc_sf)
# 
# # upper salmon  
# us_sf = us_ms_sf %>%
#   mutate(Avg_wdt = NA,
#          SHAPE_L = NA,
#          sinusty = NA) %>%
#   rbind(us_sc_sf %>%
#           mutate(A_Vg_Cv = NA,
#                  Length = NA,
#                  sinusty = NA)) %>%
#   rename(Hab_Roll = hr)
# #rm(us_ms_sf, us_sc_sf)

#-----------------------------
# fix a Lsc in the Pahsimeroi data
#-----------------------------
# ph_sf = ph_sf %>%
#   drop_na(Hab_Roll) %>%
#   rbind(ph_sf %>%
#           filter(is.na(Hab_Roll)) %>%
#           mutate_at(vars(Off_C_T:sinusty),
#                     list(~as.numeric(NA))) %>%
#           mutate(Unt_Typ = "Lsc",
#                  Sgmnt_N = NA,
#                  Hab_Roll = replace_na(Hab_Roll, 5)) )

#-----------------------------
# fix a couple channel units with wrong segment number
# looks like these channel units did not have Survey123 data for some reason
#-----------------------------
# # upper Lemhi unit 151 should be dropped
# ul_sf %<>%
#   filter(Sgmnt_N != 0)
# 
# ph_sf <- ph_sf %>%
#   filter(Sgmnt_N != 0) %>%
#   rbind(ph_sf %>%
#           filter(Sgmnt_N == 0) %>%
#           mutate(Sgmnt_N = 1,
#                  Unt_Typ = 'Pool',
#                  Prc_Fns = 90,
#                  Prc_Grv = 10,
#                  Mx_Dpth = 1.32) %>%
#           mutate_at(vars(starts_with('Undrc_')),
#                     list(~ as.numeric(0)))) %>%
#   arrange(Sgmnt_N, Unt_Nmb)

# # put it all together
# dash2018_raw = rbind(ul_sf,
#                      ll_sf,
#                      ph_sf,
#                      us_sf)

#-----------------------------
# merge data together and do some cleaning
#-----------------------------
# merge all sites
dash2018_cu = dash2018_raw %>%
  mutate(Length = ifelse(is.na(Length), SHAPE_L, Length)) %>%
  # added this calculation directly, for every channel unit
  mutate(Tot_Cov = 100 - No_Cov) %>%
  select(-SHAPE_L, -sinusty, -Nhat, - Density) %>%
  rename(Sgmnt_ID = Sgmnt_N,
         CU_ID = Unt_Nmb,
         Hab_Rch = Hab_Roll,
         Fish_Rch = Reach_Nmb,
         CU_Typ = Unt_Typ,
         Aq_Veg_Cov = A_Vg_Cv,
         Off_Chnl_Typ = Off_C_T,
         Undrc_A = Undrc_V,
         Avg_Wdth = Avg_wdt) %>%
  select(SiteNam, Sgmnt_ID, CU_ID, Hab_Rch, 
         Fish_Rch, CU_Typ, Off_Chnl_Typ, Glbl_ID,            # site & unit info
         Length, SHAPE_A, Mx_Dpth, Avg_Wdth,                 # size
         Tot_Cov, No_Cov, Aq_Veg_Cov,                        # cover
         Wod_Cnt, N_Bllst, N_ChFrm, N_Wet, Wod_Vlm, Jam_Vlm, # wood
         Undrc_C, Undrc_L, Undrc_A,                          # undercut
         d50, d84, Prc_Fns, Prc_Grv, Prc_Cbb, Prc_Bld,       # substrate
         Notes, everything()) %>%
  mutate_at(vars(Sgmnt_ID),
            list(str_pad),
            width = 2,
            pad = "0") %>%
  mutate_at(vars(CU_ID),
            list(str_pad),
            width = 3,
            pad = "0") %>%
  unite(ID, Sgmnt_ID, CU_ID, remove = F) %>%
  mutate_at(vars(Aq_Veg_Cov),
            list(replace_na),
            replace = 0) %>%
  mutate(Mx_Dpth = ifelse(CU_Typ == "Pool", Mx_Dpth, NA)) %>%
  mutate_at(vars(d50, d84),
            list(~ if_else(CU_Typ == "Riffle",
                           ., as.numeric(NA)))) %>%
  mutate_at(vars(starts_with('Prc_')),
            list(~ if_else(CU_Typ %in% c("Run", "Pool"),
                           ., as.numeric(NA)))) %>%
  mutate(CU_Typ = as.character(CU_Typ),
         CU_Typ = if_else(is.na(Off_Chnl_Typ),
                          as.character(CU_Typ),
                          if_else(Off_Chnl_Typ %in% c("Ssc", "Oca"),
                                  as.character(Off_Chnl_Typ),
                                  if_else(Off_Chnl_Typ %in% c("Pool", "Riffle", "Run", "Rapid"),
                                          "Lsc", 
                                          as.character(NA))))) %>%
  mutate_at(vars(CU_Typ),
            list(as.factor)) %>%
  arrange(SiteNam, Hab_Rch, ID) %>%
  select(SiteNam, Hab_Rch, ID, everything())

# any channel units missing a CU_Typ?
dash2018_cu %>%
  filter(is.na(CU_Typ)) %>%
  st_drop_geometry() %>%
  as.data.frame()

# which channel units have segment ID == 00?
dash2018_cu %>%
  filter(Sgmnt_ID == "00")

xtabs(~ is.na(CU_Typ), dash2018_cu)
xtabs(~ CU_Typ, dash2018_cu)
xtabs(~ CU_Typ + is.na(Off_Chnl_Typ), dash2018_cu)

dash2018_cu %>%
  filter(CU_Typ == 'Lsc',
         is.na(Off_Chnl_Typ))

# clean up some datasets
# rm(ul_sf, ll_sf, ph_sf, us_sf)

#-----------------------------
# calculate some hr scale metrics for the cus
#--------------------------
dash2018_cu_plus = dash2018_cu %>%
  group_by(SiteNam, Hab_Rch) %>%
  mutate(hr_Pool_Mx_Dpth = max(Mx_Dpth, na.rm = T),
         hr_Pool_Avg_Mx_Dpth = mean(Mx_Dpth, na.rm = T),
         hr_Pool_CV_Mx_Dpth = sd(Mx_Dpth, na.rm = T) / mean(Mx_Dpth, na.rm = T),
         hr_Tot_Cov = 100 - weighted.mean(No_Cov, SHAPE_A),
         hr_Aq_Veg_Cov = weighted.mean(Aq_Veg_Cov, SHAPE_A),
         hr_d50 = weighted.mean(d50, SHAPE_A, na.rm = T),
         hr_d84 = weighted.mean(d84, SHAPE_A, na.rm = T),
         hr_Prc_Fns = weighted.mean(Prc_Fns, SHAPE_A, na.rm = T),
         hr_Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A, na.rm = T),
         hr_Prc_Cbb = weighted.mean(Prc_Cbb, SHAPE_A, na.rm = T),
         hr_Prc_Bld = weighted.mean(Prc_Bld, SHAPE_A, na.rm = T),
         hr_CU_IDs = list(ID),
         hr_Pool_A = sum(SHAPE_A[CU_Typ == "Pool"]),
         hr_SC_A = sum(SHAPE_A[Sgmnt_ID > 1])) %>%
  ungroup() %>%
  mutate_at(vars(starts_with("hr_")),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# another way to do it, separating out metrics that are calculated similarly
dash2018_cu_plus2 = dash2018_cu %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise(hr_CU_IDs = list(ID),
                        hr_n_pool = sum(CU_Typ == 'Pool'),
                        hr_Pool_A = sum(SHAPE_A[CU_Typ == "Pool"]),
                        hr_n_SC = n_distinct(Sgmnt_ID[Sgmnt_ID > 1]),
                        hr_SC_A = sum(SHAPE_A[Sgmnt_ID > 1])) %>%
              st_drop_geometry() %>%
              as_tibble()) %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise_at(vars(Mx_Dpth),
                           lst(n_pool = ~ sum(!is.na(.)),
                               hr_Pool_Mx_Dpth = max, 
                               hr_Pool_Avg_Mx_Dpth = mean, 
                               hr_Pool_CV_Mx_Dpth = ~ sd(., na.rm = T) / mean(., na.rm = T)),
                           na.rm = T) %>%
              st_drop_geometry() %>%
              as_tibble()) %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise_at(vars(hr_Tot_Cov = Tot_Cov,
                                hr_Aq_Veg_Cov = Aq_Veg_Cov,
                                hr_d50 = d50,
                                hr_d84 = d84,
                                hr_Prc_Fns = Prc_Fns,
                                hr_Prc_Grv = Prc_Grv,
                                hr_Prc_Cbb = Prc_Cbb,
                                hr_Prc_Bld = Prc_Bld),
                           list(~ weighted.mean(., 
                                                w = SHAPE_A,
                                                na.rm = T))) %>%
              st_drop_geometry() %>%
              as_tibble()) %>%
  mutate_at(vars(starts_with("hr_")),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# are these 2 data frames identical?
dash2018_cu_plus2 %>%
  select(one_of(names(dash2018_cu_plus))) %>%
  mutate_at(vars(which(sapply(., FUN = function(x) class(x)[1]) %in% c('numeric', 'integer'))),
            list(round),
            digits = 10) %>%
  arrange(SiteNam, Hab_Rch, CU_ID) %>%
  identical(dash2018_cu_plus %>%
              mutate_at(vars(which(sapply(., FUN = function(x) class(x)[1]) %in% c('numeric', 'integer'))),
                        list(round),
                        digits = 10) %>%
              arrange(SiteNam, Hab_Rch, CU_ID))
# yes they are


# save channel unit scale data
save(dash2018_cu, dash2018_cu_plus, file = "data/prepped/dash2018_cus.Rda")

# filter and save shapefile of mainstem units
# dash2018_cu_plus_ms = dash2018_cu_plus %>%
#   filter(st_geometry_type(.) == "POLYGON")
# 
# ...and side channels
# dash2018_cu_plus_sc = dash2018_cu_plus %>%
#   filter(st_geometry_type(.) == "LINESTRING")

#-----------------------------
# plot dash2018_cu for visualization
#-----------------------------
dash_cu_plotlist = dash2018_cu_plus %>%
  mutate(SiteName = SiteNam) %>%
  group_by(SiteNam) %>%
  nest() %>%
  mutate(fig = map(data,
                   .f = function(x) {
                     x %>%
                       ggplot() +
                       geom_sf(aes(fill = CU_Typ)) +
                       guides(fill = FALSE) +
                       ggtitle(unique(x$SiteName)) +
                       theme_bw()
                   }))

dash_cu_plot = cowplot::plot_grid(plotlist = dash_cu_plotlist$fig)
dash_cu_plot

#-----------------------------
# begin to summarize data using Hab_Rch column
#--------------------------
# original calculations
dash2018_hr = dash2018_cu_plus %>%
  group_by(SiteNam, Hab_Rch) %>%
  summarise(CU_IDs = unique(hr_CU_IDs),
            Sgmnt_Cnt = n_distinct(Sgmnt_ID),
            CU_Cnt = n_distinct(CU_ID),
            Pool_Cnt = length(which(CU_Typ == "Pool")),
            Run_Cnt = length(which(CU_Typ == "Run")),
            Riffle_Cnt = length(which(CU_Typ == "Riffle")),
            Rapid_Cnt = length(which(CU_Typ == "Rapid")),
            Lsc_Cnt = length(which(CU_Typ == "Lsc")),
            Ssc_Cnt = length(which(CU_Typ == "Ssc")),
            Oca_Cnt = length(which(CU_Typ == "Oca")),
            Length = sum(Length),
            SHAPE_A = sum(SHAPE_A),
            Wod_Cnt = sum(Wod_Cnt),
            N_Bllst = sum(N_Bllst),
            N_ChFrm = sum(N_ChFrm),
            N_Wet = sum(N_Wet),
            Wod_Vlm = sum(Wod_Vlm),
            Jam_Vlm = sum(Jam_Vlm),
            Undrc_C = sum(Undrc_C),
            Undrc_L = sum(Undrc_L),
            Undrc_A = sum(Undrc_A),
            Pool_Mx_Dpth = unique(hr_Pool_Mx_Dpth),
            Pool_Avg_Mx_Dpth = unique(hr_Pool_Avg_Mx_Dpth),
            Pool_CV_Mx_Dpth = unique(hr_Pool_CV_Mx_Dpth),
            Tot_Cov = round(unique(hr_Tot_Cov), 2),
            Aq_Veg_Cov = round(unique(hr_Aq_Veg_Cov), 2),
            d50 = unique(hr_d50),
            d84 = unique(hr_d84),
            Prc_Fns = unique(hr_Prc_Fns),
            Prc_Grv = unique(hr_Prc_Grv),
            Prc_Cbb = unique(hr_Prc_Cbb),
            Prc_Bld = unique(hr_Prc_Bld),
            Pool_A = unique(hr_Pool_A),
            SC_A = unique(hr_SC_A)) %>%
  ungroup() %>%
  mutate(FstNT_Cnt = Run_Cnt,
         FstTurb_Cnt = Riffle_Cnt + Rapid_Cnt,
         Slowwater_Pct = (Pool_A / SHAPE_A) * 100,
         SC_Pct = (SC_A / SHAPE_A) * 100,
         LWFreq_Wet = (N_Wet / Length) * 100) %>%
  mutate_at(vars(-SiteNam, -Hab_Rch),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# calculating all metrics directly from cu, not cu_plus
dash2018_hr2 = dash2018_cu %>%
  group_by(SiteNam, Hab_Rch) %>%
  summarise(CU_IDs = list(unique(CU_ID)),
            Sgmnt_Cnt = n_distinct(Sgmnt_ID),
            CU_Cnt = n_distinct(CU_ID),
            Pool_Cnt = sum(CU_Typ == "Pool"),
            Run_Cnt = sum(CU_Typ == "Run"),
            Riffle_Cnt = sum(CU_Typ == "Riffle"),
            Rapid_Cnt = sum(CU_Typ == "Rapid"),
            Lsc_Cnt = sum(CU_Typ == "Lsc"),
            Ssc_Cnt = sum(CU_Typ == "Ssc"),
            Oca_Cnt = sum(CU_Typ == "Oca"),
            Length = sum(Length),
            SHAPE_A = sum(SHAPE_A),
            Wod_Cnt = sum(Wod_Cnt),
            N_Bllst = sum(N_Bllst),
            N_ChFrm = sum(N_ChFrm),
            N_Wet = sum(N_Wet),
            Wod_Vlm = sum(Wod_Vlm),
            Jam_Vlm = sum(Jam_Vlm),
            Undrc_C = sum(Undrc_C),
            Undrc_L = sum(Undrc_L),
            Undrc_A = sum(Undrc_A),
            Pool_Mx_Dpth = max(Mx_Dpth, na.rm = T),
            Pool_Avg_Mx_Dpth = mean(Mx_Dpth, na.rm = T),
            Pool_CV_Mx_Dpth = sd(Mx_Dpth, na.rm = T) / Pool_Avg_Mx_Dpth) %>%
  # not sure why this needs to be a left join, but when I ty to include all these with the code above it doesn't work
  left_join(dash2018_cu %>%
              st_drop_geometry() %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise(Tot_Cov = weighted.mean(Tot_Cov, SHAPE_A),
                        Aq_Veg_Cov = weighted.mean(Aq_Veg_Cov, SHAPE_A),
                        d50 = weighted.mean(d50, SHAPE_A, na.rm = T),
                        d84 = weighted.mean(d84, SHAPE_A, na.rm = T),
                        Prc_Fns = weighted.mean(Prc_Fns, SHAPE_A, na.rm = T),
                        Prc_Grv = weighted.mean(Prc_Grv, SHAPE_A, na.rm = T),
                        Prc_Cbb = weighted.mean(Prc_Cbb, SHAPE_A, na.rm = T),
                        Prc_Bld = weighted.mean(Prc_Bld, SHAPE_A, na.rm = T),
                        Pool_A = sum(SHAPE_A[CU_Typ == "Pool"]),
                        SC_A = sum(SHAPE_A[Sgmnt_ID > 1])) %>%
              mutate_at(vars(ends_with("Cov")),
                        list(round),
                        digits = 2)) %>%
  ungroup() %>%
  mutate(FstNT_Cnt = Run_Cnt,
         FstTurb_Cnt = Riffle_Cnt + Rapid_Cnt,
         Slowwater_Pct = (Pool_A / SHAPE_A) * 100,
         SC_Pct = (SC_A / SHAPE_A) * 100,
         LWFreq_Wet = (N_Wet / Length) * 100) %>%
  mutate_at(vars(-SiteNam, -Hab_Rch),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# another way, combining some of similarly calculated metrics
dash2018_hr3 = dash2018_cu %>%
  group_by(SiteNam, Hab_Rch) %>%
  summarise(CU_IDs = list(CU_ID),
            Sgmnt_Cnt = n_distinct(Sgmnt_ID),
            CU_Cnt = n_distinct(CU_ID)) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry() %>%
              mutate(CU_Typ = fct_relevel(CU_Typ,
                                          "Pool", "Run", 'Riffle',
                                          'Rapid', 'Lsc', 'Ssc', 'Oca')) %>%
              mutate(CU_Typ = fct_explicit_na(CU_Typ,
                                              "NA")) %>%
              group_by(SiteNam, Hab_Rch, CU_Typ) %>%
              summarise(cnt = n_distinct(CU_ID)) %>%
              ungroup() %>%
              mutate(CU_Typ = fct_relabel(CU_Typ,
                                          ~ paste(., "Cnt", sep = '_'))) %>%
              spread(CU_Typ, cnt,
                     fill = 0)) %>%
  left_join(dash2018_cu %>%
              st_drop_geometry() %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise_at(vars(Length,
                                SHAPE_A,
                                Wod_Cnt,
                                N_Bllst,
                                N_ChFrm,
                                N_Wet,
                                Wod_Vlm,
                                Jam_Vlm,
                                Undrc_C,
                                Undrc_L,
                                Undrc_A),
                           list(sum))) %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise(Pool_A = sum(SHAPE_A[CU_Typ == "Pool"]),
                        n_SC = n_distinct(Sgmnt_ID[Sgmnt_ID > 1]),
                        SC_A = sum(SHAPE_A[Sgmnt_ID > 1])) %>%
              st_drop_geometry() %>%
              as_tibble()) %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise_at(vars(Mx_Dpth),
                           lst(n_pool = ~ sum(!is.na(.)),
                               Pool_Mx_Dpth = max, 
                               Pool_Avg_Mx_Dpth = mean, 
                               Pool_CV_Mx_Dpth = ~ sd(., na.rm = T) / mean(., na.rm = T)),
                           na.rm = T) %>%
              st_drop_geometry() %>%
              as_tibble()) %>%
  left_join(dash2018_cu %>%
              group_by(SiteNam, Hab_Rch) %>%
              summarise_at(vars(Tot_Cov = Tot_Cov,
                                Aq_Veg_Cov = Aq_Veg_Cov,
                                d50 = d50,
                                d84 = d84,
                                Prc_Fns = Prc_Fns,
                                Prc_Grv = Prc_Grv,
                                Prc_Cbb = Prc_Cbb,
                                Prc_Bld = Prc_Bld),
                           list(~ weighted.mean(., 
                                                w = SHAPE_A,
                                                na.rm = T))) %>%
              st_drop_geometry() %>%
              mutate_at(vars(ends_with("Cov")),
                        list(round),
                        digits = 2) %>%
              as_tibble()) %>%
  ungroup() %>%
  mutate(FstNT_Cnt = Run_Cnt,
         FstTurb_Cnt = Riffle_Cnt + Rapid_Cnt,
         Slowwater_Pct = (Pool_A / SHAPE_A) * 100,
         SC_Pct = (SC_A / SHAPE_A) * 100,
         LWFreq_Wet = (N_Wet / Length) * 100) %>%
  mutate_at(vars(-SiteNam, -Hab_Rch),
            ~replace(., . %in% c("NaN", "-Inf"), NA))

# are these results the same?
# dash2018_hr2 %>%
dash2018_hr3 %>%
  select(one_of(names(dash2018_hr))) %>%
  st_drop_geometry() %>%
  select(-CU_IDs) %>%
  mutate_at(vars(which(sapply(., FUN = function(x) class(x)[1]) %in% c('numeric', 'integer'))),
            list(round),
            digits = 5) %>%
  identical(dash2018_hr %>%
              st_drop_geometry() %>%
              select(-CU_IDs) %>%
              mutate_at(vars(which(sapply(., FUN = function(x) class(x)[1]) %in% c('numeric', 'integer'))),
                        list(round),
                        digits = 5))



# Others to consider?
#Ssc_Avg_Wdth = weighted.mean(Avg_Wdth, Length) if(Off_chnl_Typ == "Ssc")

#-----------------------------
# calculate sinuosity and braidedness metrics and add to dash2018_hr
#-----------------------------

# read in the centerlines with Hab_Roll column
ul_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/UL_Centerline_sin.shp")
ll_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/LL_Centerline_sin.shp")
ph_cl = st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/Pah_Centerline_sin.shp")
us_cl <- st_read("data/raw/dash2018/ShapefilesWith_MetricsV3_reaches_RC/US_Centerline_sin.shp") %>%
  st_transform(crs = st_crs(mra_crs))
  
# combine the above centerlines (still need to resolve the Pahsimeroi Hab_Roll 5)
mra_cl = rbind(ul_cl,
               ll_cl,
               ph_cl,
               us_cl) %>%
  # fix the Pahsimeroi NA Hab_Roll which belongs in Hab_Roll 5
  mutate(Hab_Roll = replace_na(Hab_Roll, 5)) %>%
  select("SiteNam", "Hab_Roll", "sinuosity") %>%
  mutate(Length = as.numeric(st_length(geometry))) %>%
  mutate(straight_line = map_dbl(geometry,
                                 .f = function(x) {
                                   st_distance(st_line_sample(x, sample = 0),
                                               st_line_sample(x, sample = 1))
                                 })) %>%
  mutate(sinuosity = straight_line / Length) %>%
  st_drop_geometry() %>%
  as_tibble()

# combine the side channels and calculate sc length by Hab_Roll
mra_sc = ul_sc_sf %>%
  mutate(A_Vg_Cv = NA,
         Length = NA) %>%
  rbind(ll_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA)) %>%
  rbind(ph_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA)) %>%
  rbind(us_sc_sf %>%
          mutate(A_Vg_Cv = NA,
                 Length = NA,
                 sinusty = NA) %>%
          rename(Hab_Roll = hr)) %>%
  select("SiteNam", "Hab_Roll") %>%
  mutate(Length = as.numeric(st_length(geometry))) %>%
  group_by(SiteNam, Hab_Roll) %>%
  summarise(sc_Length = sum(Length)) %>%
  st_drop_geometry() %>%
  as_tibble()

# now calculate some sinuosity and braidedness metrics by Hab_Roll
hr_sin_wet_braid = mra_cl %>%
  left_join(mra_sc,
            by = c("SiteNam", "Hab_Roll")) %>%
  mutate_at(vars(sc_Length), 
            list(replace_na), 
            replace = 0) %>%
  mutate(Tot_Length = Length + sc_Length) %>%
  mutate(wet_braid = Tot_Length / Length) %>%
  mutate(wet_braid_sin = Tot_Length / straight_line) %>%
  rename(Hab_Rch = Hab_Roll)

# join to dash2018_hr
dash2018_hr = dash2018_hr %>%
  left_join(hr_sin_wet_braid, 
            by = c("SiteNam", "Hab_Rch"))

# save habitat reach scale data
save(dash2018_hr, file = "data/prepped/dash2018_hr.Rda")
# st_write(dash2018_hr, "dash2018_hr.shp")
  
# clean up some datasets
#rm(ul_cl, ll_cl, ph_cl, us_cl)

#-----------------------------------------------------
# read in Morgan's data, select attributes, and join to the dash2018_hr
#-----------------------------------------------------
# gaa, norwest, natdist, from Salmon basin only. Richie's NAS is mapped to Z:/...Mike's is S:/
load("Z:/habitat/full_join/SalmonBasin/salmon_full_join.Rda")
load("S:/habitat/full_join/SalmonBasin/salmon_full_join.Rda")

# transform to the same crs
salmon_full_join = salmon_full_join %>%
  st_transform(crs = st_crs(mra_crs))

# select attributes to join
salmon_gaa_trim = salmon_full_join %>%
  select("UniqueID",
         "S2_02_11.y",
         "NatPrin1",
         "NatPrin2",
         "DistPrin1",
         "MeanU_v1")

# plot salmon_gaa_trim
ggplot() +
  geom_sf(data = salmon_gaa_trim,
          aes(fill = UniqueID)) +
  labs(title = "GAA Data",
       fill = "Site ID") +
  theme_bw()

# what is the nearest gaa to each dash2018_fr?
st_nearest_feature(dash2018_hr,
                   salmon_gaa_trim)

# join nearest gaa to the dash2018_fr
dash2018_hr_gaa = dash2018_hr %>%
  st_join(salmon_gaa_trim,
          join = st_nearest_feature,
          left = TRUE)

# save habitat reach scale data
save(dash2018_hr, dash2018_hr_gaa, file = "data/prepped/dash2018_hr.Rda")
