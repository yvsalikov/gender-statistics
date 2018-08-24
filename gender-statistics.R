library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gghighlight)
library(sf)
library(rnaturalearth)
library(viridis)
library(leaflet)

# Auxiliary function to extract data 
get_indicator <- function(data, indicator.name, column.name) {
  # world.bank.data from Global Env.
  x <- world.bank.data %>% 
    filter(Indicator.Name==indicator.name) %>%
    select(Country.Code, "X1960":"X2016") %>% 
    gather(key="Year", value=!!column.name, "X1960":"X2016") %>% 
    na.omit(cols=!!column.name) %>% 
    mutate(Year=as.numeric(str_sub(Year, 2, -1)))
  data <- full_join(data, x, by=c("Country.Code", "Year"))
  
  data
}


# Import data from World Bank ---------------------------------------------
# https://datacatalog.worldbank.org/dataset/gender-statistics
# do not filter anithing
world.bank.data <- read.csv("data/Gender_StatsData.csv", fileEncoding = "UTF-8-BOM")

gender.statistics <- data.frame(Country.Code= character(0), Year= integer(0))
gender.statistics <- gender.statistics %>% 
  get_indicator("Fertility rate, total (births per woman)", "Fertility.rate") %>% 
  get_indicator("GDP per capita (Current US$)", "GDP.per.capita") %>% 
  get_indicator("Birth rate, crude (per 1,000 people)", "Birth.rate") %>% 
  get_indicator("Death rate, crude (per 1,000 people)", "Death.rate")


# Importing geometries from Natural Earth ---------------------------------
worldgeoms <- st_as_sf(countries110) %>% 
  filter(sovereignt!="Antarctica") %>% #exclude Antarctica
  select(sovereignt, iso_a3, income_grp, geometry)

#join data with geometries
gender.statistics <- left_join(worldgeoms, gender.statistics, by=c("iso_a3"="Country.Code"))

# Various plots -----------------------------------------------------------

## Birth Rate trends for income groups
ggplot(gender.statistics) +
  #geom_point() +
  geom_smooth(aes(x=Year, y=Birth.rate, group=income_grp, colour=income_grp)) +
  theme_minimal()

## Death Rate trends for income groups
ggplot(gender.statistics) +
  #geom_point() +
  geom_smooth(aes(x=Year, y=Death.rate, group=income_grp, colour=income_grp)) +
  theme_minimal()

## GDP per Capita trends for income groups
ggplot(gender.statistics) +
  #geom_point() +
  geom_smooth(aes(x=Year, y=GDP.per.capita, group=income_grp, colour=income_grp)) +
  theme_minimal()

## Birth Rates vs. GDP per Capita in 2016
gender.statistics.2016 <- filter(gender.statistics, Year==2016)
ggplot() +
  geom_point(data=gender.statistics.2016, aes(x=GDP.per.capita, y=Fertility.rate, colour=income_grp, shape=income_grp)) +
  geom_smooth(data=gender.statistics.2016, aes(x=GDP.per.capita, y=Fertility.rate, alfa=0.5)) +
  #gghighlight_point(max(Fertility.rate), max_highlight = 1L) +
  theme_minimal()

df <- gender.statistics %>% 
  filter(income_grp=="1. High income: OECD") %>% 
  #group_by(iso_a3) %>% 
  arrange(iso_a3, Year) %>% 
  mutate(d.Fertility.rate=Fertility.rate-lag(Fertility.rate)) %>% 
  mutate(d.GDP.per.capita=GDP.per.capita-lag(GDP.per.capita))


gender.statistics.2016 <- mutate(gender.statistics.2016, Natural.Increase=Birth.rate-Death.rate)

# Leaflet -----------------------------------------------------------------
pal <- colorNumeric("YlOrRd", domain=gender.statistics.2016$Fertility.rate)
labels <- paste0("<b>", gender.statistics.2016$sovereignt,"</b><br/>",
                 "<i>", str_sub(gender.statistics.2016$income_grp, 3, -1), "</i><br/>",
                 as.character(round(gender.statistics.2016$Fertility.rate, 1))," birth per woman.") %>% 
          lapply(htmltools::HTML)

leaflet(gender.statistics.2016) %>% 
  addTiles() %>%   # default OpenStreetMap map tiles
  addPolygons(fillColor = ~pal(Fertility.rate), 
              fillOpacity = 0.7, 
              weight = 1, 
              smoothFactor = 0.2,
              color = "white",
              highlight = highlightOptions(weight = 3,
                                           color = "green",
                                           fillOpacity = 1,
                                           bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
  ) %>% 
  addLegend(pal = pal, values = ~Fertility.rate, title = NULL, position = "bottomleft")

# Choropleth maps ---------------------------------------------------------
ggplot(gender.statistics.2016) +
  geom_sf(col="grey", aes(fill=Natural.Increase)) +
  coord_sf(crs="+proj=robin") +
  scale_fill_viridis(option = "magma",
                     name='Natural \nincrease',
                     direction = -1,
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = 0.5,
                       barwidth = 10,
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position="bottom", legend.box = "horizontal") +
  labs(x=NULL,
       y=NULL,
       title="Rate of natural increase in 2016",
       subtitle="Course \"Introduction to R for Journalists\"",
       caption='Source: Gender Statistics, The World Bank. Map: Natural Earth.')

ggplot(gender.statistics.2016) +
  geom_sf(col="grey", fill=NA) +
  geom_sf(col="grey", aes(fill=Fertility.rate)) +
  coord_sf(crs="+proj=robin") +
  scale_fill_viridis(option = "magma",
                     name='Births \nper woman',
                     direction = -1,
                     guide = guide_colorbar(
                       direction = "horizontal",
                       barheight = 0.5,
                       barwidth = 10,
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     )) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(legend.position=c(0.75, 0.2), legend.box = "horizontal") +
  labs(x=NULL,
       y=NULL,
       title="Fertility rate (births per woman) in countries by income, 2016",
       subtitle="Course \"Introduction to R for Journalists\"",
       caption='Source: Gender Statistics, The World Bank. Map: Natural Earth.') +
  facet_wrap(~income_grp, ncol=2)

