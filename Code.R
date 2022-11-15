rm(list=ls())


library(sf)
library(raster)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)    
library(leaflet) 



# Data
# Source (open data Zurich)
df <- read.csv("data/BAU516OD5161.csv")

# Load shape files
shapefile <- read_sf('shape2/data/stzh.adm_statzonen_map.shp')


# 2 rooms flat
df_2z <- df %>%
  filter(ZimmerSort==2, EinheitLang=='Wohnung', PreisartLang=='Netto',
         RaumeinheitLang=='Stadtkreise', GemeinnuetzigSort==0) %>%
  mutate(zone=GliederungSort/10) %>%
  mutate(Mean=mean) %>%
  select(zone, Mean)

df_shape_2z <- shapefile %>%
  left_join(df_2z, by = c("knr"="zone"))

m1 <- tm_shape(df_shape_2z)+
  tm_polygons("Mean", n=5, legend.show = TRUE, palette="Reds",title="Rental average")+
  tm_style("albatross")+
  tm_layout(
    title="Two-room flat in Zurich",
    title.size = 1.2,
    title.position = c('left','top'),
    legend.title.size = 1,
    legend.text.size = .6,
    legend.position = c(.85, .2),
    inner.margins = c(0, .15, .15, .2)
  )+
  tm_credits(text = "Data: Stadt Zürich | Open data (Apr. 2022)", position = c('left','bottom'))
m1
tmap_save(filename = 'plot_2z.png',width = 8,height = 8,dpi = 320)


# 3 rooms flat
df_3z <- df %>%
  filter(ZimmerSort==3, EinheitLang=='Wohnung', PreisartLang=='Netto',
         RaumeinheitLang=='Stadtkreise', GemeinnuetzigSort==0) %>%
  mutate(zone=GliederungSort/10) %>%
  mutate(Mean=mean) %>%
  select(zone, Mean)

df_shape_3z <- shapefile %>%
  left_join(df_3z, by = c("knr"="zone"))

m2 <- tm_shape(df_shape_3z)+
  tm_polygons("Mean", n=5, legend.show = TRUE, palette="Reds",title="Rental average")+
  tm_style("albatross")+
  tm_layout(
    title="Three-room flat in Zurich",
    title.size = 1.2,
    title.position = c('left','top'),
    legend.title.size = 1,
    legend.text.size = .6,
    legend.position = c(.85, .2),
    inner.margins = c(0, .15, .15, .2)
  )+
  tm_credits(text = "Data: Stadt Zürich | Open data (Apr. 2022)", position = c('left','bottom'))
m2
tmap_save(filename = 'plot_3z.png',width = 8,height = 8,dpi = 320)


# 4 rooms flat
df_4z <- df %>%
  filter(ZimmerSort==4, EinheitLang=='Wohnung', PreisartLang=='Netto',
         RaumeinheitLang=='Stadtkreise', GemeinnuetzigSort==0) %>%
  mutate(zone=GliederungSort/10) %>%
  mutate(Mean=mean) %>%
  select(zone, Mean)

df_shape_4z <- shapefile %>%
  left_join(df_4z, by = c("knr"="zone"))

m3 <- tm_shape(df_shape_4z)+
  tm_polygons("Mean", n=8, legend.show = TRUE, palette="Reds",title="Rental average")+
  tm_style("albatross")+
  tm_layout(
    title="Four-room flat in Zurich",
    title.size = 1.2,
    title.position = c('left','top'),
    legend.title.size = 1,
    legend.text.size = .6,
    legend.position = c(.85, .2),
    inner.margins = c(0, .15, .15, .2)
  )+
  tm_credits(text = "Data: Stadt Zürich | Open data (Apr. 2022)", position = c('left','bottom'))
m3
tmap_save(filename = 'plot_4z.png',width = 8,height = 8,dpi = 320)

