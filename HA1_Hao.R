# ----
library(tidytransit)
library(tidyverse)

#
setwd("/Users/haowu/Documents/Planung_und_Betrieb_im_Verkehrswesen/WS2020/Data Science for agent-based transport simulations WiSe 20:21/Homework1/Hao")

gtfs_vbb_2020 <- read_gtfs("BVG_VBB_bereichsscharf.zip")
gtfs_vbb_2020_12 <- read_gtfs("2020-12_2020-12-28.zip")
#gtfs_vbb_2020_02 <- read_gtfs("2020-02_2020-02-27.zip")
#gtfs_vbb_2019_05 <- read_gtfs("2019-05_2019-05-13.zip")
#gtfs_vbb_2019_06 <- read_gtfs("2019-06_2019-06-04.zip")

#
trips <- gtfs_vbb_2020_12$trips
routes <- gtfs_vbb_2020_12$routes
stop_times <- gtfs_vbb_2020_12$stop_times

head(trips)
head(routes)
head(stop_times)

#
shapes <- gtfs_vbb_2020_12$shapes
stops <- gtfs_vbb_2020_12$stops

stops %>% 
  group_by(stop_desc) %>%
  #summarise(mean = mean(arr_delay, na.rm = TRUE)) %>% 
  #left_join(airports, c("dest" = "faa")) %>% 
  ggplot(aes("shape_pt_lon", "shape_pt_lat")) +
  #borders("shape_id") +
  geom_point() +
  coord_quickmap() 




# traffic accidents Berlin ----
accidents <- read_delim("TrafficAccidents_Berlin/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv",
                        delim=";", 
                        locale=locale(decimal_mark = "."))

# Billy给的 ----
accidents <- readr::read_delim("/Users/haowu/Documents/Planung_und_Betrieb_im_Verkehrswesen/WS2020/Data Science for agent-based transport simulations WiSe 20:21/Homework1/Hao/TrafficAccidents_Berlin/AfSBBB_BE_LOR_Strasse_Strassenverkehrsunfaelle_2019_Datensatz.csv", locale = locale(encoding = "latin1"), delim=";")

# For Western European text, typical encoding types can be "latin1", "ISO-8859-1", or "UTF-16LE". 
# Try one of those until your text is imported correctly.
# You can find more information about locales and encoding by reading in RStudio: ?read_delim and vignette("locale")






# shape new -----
require(sf)
require(tidytransit)
require(tidyverse)
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

# setwd("/Users/haowu/Documents/Planung_und_Betrieb_im_Verkehrswesen/WS2020/Data Science for agent-based transport simulations WiSe 20:21/Homework1/Hao/shapeFile")
# shape_districts_new <- read_sf(dsn = "LOR_SHP_2019-1", layer = "Planungsraum_EPSG_25833")
shape_districts_new <- read_sf(dsn = "shapeFile/LOR_SHP_2019-1", layer = "Planungsraum_EPSG_25833")
# shape_districts <- read_sf(dsn = "shp-bezirke", layer = "bezirke_berlin")
## setwd("/home/misax/Documents/Uni/Master/DataScienceTransport")

shape_districts_new <- shape_districts_new %>% 
  group_by(BEZIRK) %>% 
  summarise() %>% 
  filter(!is.na(BEZIRK)) %>% 
  rename(NAME = BEZIRK) %>% 
  mutate(AREA = st_area(geometry)) %>% 
  select(NAME, AREA, everything())

plot_districts_new <- ggplot() + 
  geom_sf(data = shape_districts_new,
          aes(fill = NAME),
          #col = sf.colors(23, categorical = FALSE)
  ) +
  #theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none"
  )

plot_districts_new
# plot(shape_districts_new)

# shape old(billies shape file) -----
# setwd("/Users/haowu/Documents/Planung_und_Betrieb_im_Verkehrswesen/WS2020/Data Science for agent-based transport simulations WiSe 20:21/Homework1/Hao/shapeFile")
# shape_districts <- read_sf(dsn = "shapeFile/shp-bezirke", layer = "bezirke_berlin")
shape_districts <- read_sf(dsn = "shp-bezirke", layer = "bezirke_berlin")
# setwd("/home/misax/Documents/Uni/Master/DataScienceTransport")

# plot the district
# shape_districts %>% 
#   filter(SCHLUESSEL == "060305") %>% 
#   plot()

#set district names
shape_districts <- shape_districts %>%
  mutate(NAME = case_when(SCHLUESSEL == "060305" ~ "Zehlendorf",
                          SCHLUESSEL == "060203" ~ "Steglitz",
                          SCHLUESSEL == "050102" ~ "Spandau",
                          SCHLUESSEL == "123012" ~ "Reinickendorf",
                          SCHLUESSEL == "030405" ~ "Pankow",
                          SCHLUESSEL == "030509" ~ "Weissensee",
                          SCHLUESSEL == "110101" ~ "Hohenschoenhausen",
                          SCHLUESSEL == "010331" ~ "Wedding",
                          SCHLUESSEL == "040413" ~ "Wilmersdorf",
                          SCHLUESSEL == "040309" ~ "Charlottenburg",
                          SCHLUESSEL == "070303" ~ "Schoeneberg",
                          SCHLUESSEL == "070707" ~ "Tempelhof",
                          SCHLUESSEL == "080105" ~ "Neukoelln",
                          SCHLUESSEL == "010111" ~ "Tiergarten",
                          SCHLUESSEL == "030611" ~ "Prenzlauer Berg",
                          SCHLUESSEL == "100103" ~ "Mahrzahn",
                          SCHLUESSEL == "100204" ~ "Hellersdorf",
                          SCHLUESSEL == "090520" ~ "Koepenick",
                          SCHLUESSEL == "090101" ~ "Treptow",
                          SCHLUESSEL == "110410" ~ "Lichtenberg",
                          SCHLUESSEL == "020405" ~ "Friedrichshain",
                          SCHLUESSEL == "010113" ~ "Mitte",
                          SCHLUESSEL == "020101" ~ "Kreuzberg"
  )
  ) %>% 
  rename(KEY = SCHLUESSEL) %>% 
  mutate(AREA = st_area(geometry)) %>% 
  select(NAME, AREA, everything())

# Plot
# https://r-spatial.github.io/sf/articles/sf5.html
plot_districts <- ggplot() + 
  geom_sf(data = shape_districts,
          aes(fill = NAME),
          #col = sf.colors(23, categorical = FALSE)
  ) +
  #theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5) 
  )

plot_districts










# Data basedCase ----
baseCase_trips <-read_delim("NoCarZone/berlin-v5.5-1pct.output_trips_baseCase.csv.gz", 
                             delim=";", 
                             locale=locale(decimal_mark = "."),
                             col_types = cols(
                               #departureTime = col_double(),
                               person = col_double(),
                               #vehicleId = col_character(),
                               #fromLinkId = col_double(),
                               start_x = col_double(),
                               start_y = col_double()
                               #toLinkId = col_double(),
                               #toX = col_double(),
                               #toY = col_double(),
                               #waitTime = col_double(),
                               #arrivalTime = col_double(),
                               #travelTime = col_double(),
                               #travelDistance_m = col_double(),
                               #direcTravelDistance_m = col_double()
                             ))

baseCase_persons <-read_delim("NoCarZone/berlin-v5.5-1pct.output_persons_baseCase.csv.gz", 
                               delim=";", 
                               locale=locale(decimal_mark = "."),
                               col_types = cols(
                                 person = col_double(),
                                 first_act_x = col_double(),
                                 first_act_y = col_double(),
                                 subpopulation = col_character(),
                                 home_activity_zone = col_character()
                               ))


# Data nocarzone ----
nocarzone_trips <-read_delim("NoCarZone/berlin-v5.5-1pct.output_trips_nocarzone.csv.gz", 
                             delim=";", 
                             locale=locale(decimal_mark = "."),
                             col_types = cols(
                               #departureTime = col_double(),
                               person = col_double(),
                               #vehicleId = col_character(),
                               #fromLinkId = col_double(),
                               start_x = col_double(),
                               start_y = col_double(),
                               #toLinkId = col_double(),
                               end_x = col_double(),
                               end_y = col_double(),
                               longest_distance_mode = col_character(),
                               modes = col_character()
                               #waitTime = col_double(),
                               #arrivalTime = col_double(),
                               #travelTime = col_double(),
                               #travelDistance_m = col_double(),
                               #direcTravelDistance_m = col_double()
                             ))

nocarzone_persons <-read_delim("NoCarZone/berlin-v5.5-1pct.output_persons_nocarzone.csv.gz", 
                             delim=";", 
                             locale=locale(decimal_mark = "."),
                             col_types = cols(
                               person = col_double(),
                               first_act_x = col_double(),
                               first_act_y = col_double(),
                               subpopulation = col_character(),
                               home_activity_zone = col_character()
                             ))


nocarzone_affectedAgents <-read_delim("NoCarZone/berlin-v5.5-1pct.output_affectedAgents_nocarzone.csv.gz", 
                                      delim=";", 
                                      locale=locale(decimal_mark = "."),
                                      col_types = cols(
                                        person = col_double()
                                      ))


# baseCase(like Plot1) -----
ggplot(data=baseCase_trips, mapping=aes(x=start_x, y=start_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="red") +
  coord_quickmap() +
  coord_fixed()

ggplot(data=baseCase_trips, mapping=aes(x=end_x, y=end_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="blue") +
  coord_quickmap() +
  coord_fixed()




# Plot1 -----
ggplot(data=nocarzone_trips, mapping=aes(x=start_x, y=start_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="red") +
  coord_quickmap() +
  coord_fixed()

ggplot(data=nocarzone_trips, mapping=aes(x=end_x, y=end_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="blue") +
  coord_quickmap() +
  coord_fixed()

# Plot1增强版 -----
view(nocarzone_persons)

# Plot1增强1版
ggplot(data=nocarzone_persons, mapping=aes(x=first_act_x, y=first_act_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="blue") +
  coord_quickmap() +
  coord_fixed()

# Plot1增强2版
nocarzone_persons %>%
 filter(subpopulation!="freight") %>%
  # group_by("subpopulation") %>%
  ggplot(mapping=aes(x=first_act_x, y=first_act_y)) +
    geom_point(mapping = aes(color=subpopulation)) +
    coord_quickmap() +
    coord_fixed()

# Plot1增强3版
nocarzone_persons %>%
  filter(subpopulation!="freight") %>%
  # group_by("subpopulation") %>%
  ggplot(mapping=aes(x=first_act_x, y=first_act_y, fill=subpopulation)) +
    geom_hex(binwidth=500) +
    geom_point(size=1.9, alpha=0.0001) +
    coord_quickmap() +
    coord_fixed()

# Plot1增强3版 + Map -----
nocarzone_trips_filter <- nocarzone_trips %>%
  filter(longest_distance_mode!="freight") %>%
  filter(longest_distance_mode!="carInternal")

require(sf)
shape_hundekopf <- read_sf(dsn = "shapeFile/berlin_hundekopf", layer = "berlin_hundekopf")

nocarzone_persons_shape <- nocarzone_persons %>%
  filter(subpopulation!="freight")
  # group_by("subpopulation") %>%
ggplot() +
  geom_hex(data=nocarzone_persons_shape, mapping=aes(x=first_act_x, y=first_act_y, fill=subpopulation), binwidth=500) +
  geom_point(data=nocarzone_persons_shape, mapping=aes(x=first_act_x, y=first_act_y, fill=subpopulation), size=1.9, alpha=0.0001) +
  coord_quickmap() +
  coord_fixed() +
  #geom_sf(data = shape_hundekopf, alpha=0.01, colour = "dark gray") 
  geom_sf(data = shape_hundekopf, alpha=0.01, colour = "#5E5E5E") +
  labs(
    title = "OD-Aggregation Analysis: Where do people live?",
    subtitle = "Berlin and Brandenburg",
    fill = "subgroup of people"
  ) +
labs(x = "The x-value of the coordinates of Agent's home", y = "The y-value of the coordinates of Agent's home")


# 有问题: Plot1 + Map -----
# shape new ---
require(sf)
require(tidytransit)
require(tidyverse)
Sys.setlocale("LC_ALL", "de_DE.UTF-8")

shape_districts_new <- read_sf(dsn = "shapeFile/LOR_SHP_2019-1", layer = "Planungsraum_EPSG_25833")


shape_districts_new <- shape_districts_new %>% 
  group_by(BEZIRK) %>% 
  summarise() %>% 
  filter(!is.na(BEZIRK)) %>% 
  rename(NAME = BEZIRK) %>% 
  mutate(AREA = st_area(geometry)) %>% 
  select(NAME, AREA, everything())




plot_districts_new <- ggplot() + 
  geom_sf(data = shape_districts_new,
          aes(fill = NAME),
          #col = sf.colors(23, categorical = FALSE)
  ) +
  #theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #legend.position="none"
  )

plot_districts_new




ggplot(data=nocarzone_persons, mapping=aes(x=first_act_x, y=first_act_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, color="blue") +
  coord_quickmap() +
  coord_fixed()




# Plot1-2废料 -----
ggplot(data=nocarzone_trips, mapping=aes(x=end_x, y=end_y, color=longest_distance_mode)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1) +
  coord_quickmap() +
  coord_fixed()

nocarzone_trips %>%
  group_by("mode") %>%
  ggplot(mapping=aes(x=end_x, y=end_y)) +
  geom_hex(binwidth=500) +
  geom_point(size=0.1, alpha=0.1, aes(x=end_x, y=end_y, color="mode")) +
  coord_quickmap() +
  coord_fixed() +
  theme(legend.key.size=unit(2,'cm'));
# theme(legend.text = element_text(colour = 'red', angle = 45, size = 1, hjust = 3, vjust = 3, face = 'bold'))





# Plot2 -----
ggplot(data=nocarzone_trips) +
  # geom_hex(binwidth=500) +
  geom_point(mapping=aes(x=start_x, y=start_y, color=longest_distance_mode)) +
  coord_quickmap() +
  coord_fixed()

nocarzone_trips %>%
  filter(longest_distance_mode!="freight") %>%
  # filter(longest_distance_mode!="pt") %>%
  ggplot() +
  # geom_hex(binwidth=500) +
  geom_point(mapping=aes(x=start_x, y=start_y, color=longest_distance_mode)) +
  coord_quickmap() +
  coord_fixed()


# Plot2 + Map -----
baseCase_trips_filter <- baseCase_trips %>%
  filter(longest_distance_mode!="freight") #%>%
  # filter(longest_distance_mode!="carInternal")
  
require(sf)
shape_hundekopf <- read_sf(dsn = "shapeFile/berlin_hundekopf", layer = "berlin_hundekopf")

ggplot() +
  geom_point(data=baseCase_trips_filter, size=0.5, mapping=aes(x=start_x, y=start_y, color=longest_distance_mode)) +
  coord_quickmap() +
  coord_fixed() +
  geom_sf(data = shape_hundekopf, alpha=0.05, colour = "#5E5E5E")  +
  labs(
    title = "Which main transportation mode will people choose?",
    subtitle = "Berlin and Brandenburg",
    fill = "transportation mode that people had chosen for the longest-distance leg"
  ) +
labs(x = "The x-value of the coordinates of Agent's home", y = "The y-value of the coordinates of Agent's home")


?geom_sf
view(shape_hundekopf)


# Plot3 -----
ggplot(data=nocarzone_trips) +
  # geom_hex(binwidth=500) +
  geom_bar(aes(modes, fill = longest_distance_mode)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))

  geom_point(mapping=aes(x=start_x, y=start_y, color=longest_distance_mode)) +
  coord_quickmap() +
  coord_fixed()

  
  
  
  

# Plot4# 解释Plot2 allAgents_baseCase-----
# nocarzone_persons_withoutFreight <- nocarzone_persons %>%
    # filter(subpopulation!="freight")


baseCase_trips_persons <- baseCase_trips %>% 
  full_join(baseCase_persons, by = c("person"), na.rm = TRUE) %>% 
  filter(subpopulation!="freight") %>%  
  filter(longest_distance_mode!="NA")
# nocarzone_trips_persons = nocarzone_trips_persons.dropna(how='all')
# view(head(nocarzone_trips_persons_withoutFreight))

# nocarzone_trips %>% 
  # filter(Bundesland == "Berlin") %>% 
  # separate(Meldedatum, into = c("year", "month", "day", "hour", "minute", "second")) %>% 
baseCase_trips_persons <- baseCase_trips_persons %>% 
  group_by(longest_distance_mode) %>% 
  summarize(count = n()) %>% 
  mutate(percent = count/sum(count))
ggplot(baseCase_trips_persons, aes(x=longest_distance_mode, y=count, label = paste0(round(percent*100,3),"%"))) +
#ggplot(data=na.omit(data["nocarzone_trips_persons_withoutFreight"])) +
  geom_bar(stat="identity", fill=c('#E26860','#93962F','#54B170','#479EEC','#CD59E8')) + 
  # coord_flip() +
  coord_polar() +
  # geom_bar(stat="identity", aes(fill = home_activity_zone)) + 
  geom_text(aes(y = count*1.1)) +
  labs(
    title = "Modal Split of Base Case",
    subtitle = "Berlin and Brandenburg",
    fill = "home_activity_zone"
  )

# Plot4# 解释Plot2 allAgents_nocarzone: 柱形图
nocarzone_trips_persons <- nocarzone_trips %>% 
  full_join(nocarzone_persons, by = c("person"), na.rm = TRUE) %>% 
  filter(subpopulation!="freight") %>%  
  filter(longest_distance_mode!="NA")
# nocarzone_trips_persons = nocarzone_trips_persons.dropna(how='all')
# view(head(nocarzone_trips_persons_withoutFreight))

# nocarzone_trips %>% 
# filter(Bundesland == "Berlin") %>% 
# separate(Meldedatum, into = c("year", "month", "day", "hour", "minute", "second")) %>% 
ggplot(data=nocarzone_trips_persons) +
  #ggplot(data=na.omit(data["nocarzone_trips_persons_withoutFreight"])) +
  geom_bar(aes(longest_distance_mode, fill = home_activity_zone)) +
  coord_flip() +
  # coord_polar() +
  labs(
    title = "Modal Split of Car-free Zone Case",
    subtitle = "Berlin and Brandenburg",
    fill = "home_activity_zone"
  ) +
  theme(axis.title.x =element_text(size=14), axis.title.y=element_text(size=14))


# Plot5# 解释Plot2 affectedAgents_Car-freeCase -----
baseCase_trips_persons <- baseCase_trips %>% 
  full_join(nocarzone_persons, by = c("person"), na.rm = TRUE) %>% 
  filter(subpopulation!="freight") %>%  
  filter(longest_distance_mode!="NA")

baseCase_trips_affectedAgents <- baseCase_trips_persons %>% 
  inner_join(nocarzone_affectedAgents, by = c("person"), na.rm = TRUE) #%>% 
# filter(subpopulation!="freight") %>%  
# filter(longest_distance_mode!="NA")

# 接下来的和Plot2#一模一样
# nocarzone_trips %>% 
# filter(Bundesland == "Berlin") %>% 
# separate(Meldedatum, into = c("year", "month", "day", "hour", "minute", "second")) %>% 
ggplot(data=baseCase_trips_affectedAgents) +
  #ggplot(data=na.omit(data["nocarzone_trips_persons_withoutFreight"])) +
  geom_bar(aes(longest_distance_mode, fill = home_activity_zone)) +
  coord_flip() +
  # coord_polar() +
  ylim(0,2200) +
  labs(
    title = "Modal Split of affected Agents in Base Case",
    subtitle = "Berlin and Brandenburg",
    fill = "home_activity_zone"
  )

# Plot5# 解释Plot2 affectedAgents_baseCase
nocarzone_trips_persons <- nocarzone_trips %>% 
  full_join(nocarzone_persons, by = c("person"), na.rm = TRUE) %>% 
  filter(subpopulation!="freight") %>%  
  filter(longest_distance_mode!="NA")

nocarzone_trips_affectedAgents <- nocarzone_trips_persons %>% 
  inner_join(nocarzone_affectedAgents, by = c("person"), na.rm = TRUE) #%>% 
  # filter(subpopulation!="freight") %>%  
  # filter(longest_distance_mode!="NA")

# 接下来的和Plot2#一模一样
# nocarzone_trips %>% 
# filter(Bundesland == "Berlin") %>% 
# separate(Meldedatum, into = c("year", "month", "day", "hour", "minute", "second")) %>% 
ggplot(data=nocarzone_trips_affectedAgents) +
  #ggplot(data=na.omit(data["nocarzone_trips_persons_withoutFreight"])) +
  geom_bar(aes(longest_distance_mode, fill = home_activity_zone)) +
  coord_flip() +
  # coord_polar() +
  ylim(0,2200) +
  labs(
    title = "Modal Split of affected Agents in Car-free Case",
    subtitle = "Berlin and Brandenburg",
    fill = "home_activity_zone"
  )
