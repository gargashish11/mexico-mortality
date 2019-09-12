pacman::p_load(foreign, stringr, ggplot2, dplyr, plyr, lubridate)
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
options(stringsAsFactors = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

deaths <- read.csv("../deaths/deaths08.csv.bz2", header = TRUE)
codes <- read.csv("../disease/icd-main.csv", header = TRUE)
locations <-
  read.csv("../locations/locations.csv.bz2", header = TRUE)
states <- read.csv("../maps/state-map.csv.bz2", header = TRUE)
daily <- read.csv("../weather-simat2/weather-daily.csv",header = TRUE)

cause <- arrange(count(deaths, "cod"), desc(freq))

names(cause)[1] <- "code"
cause$disease <- with(cause,codes$disease[match(code,codes$code)])

# attach(cause)
# ggplot(cause,aes(x=freq,y=disease)) +
#               geom_point()+
#         scale_x_log10()

# top20 diseases ----------------------------------------------------------

top20 <- head(cause, 20)
ggplot(top20, aes(freq / 1e4, reorder(disease, freq))) +
  geom_point() +
  scale_x_log10("deaths (x 10,000)", breaks = 1:5)

deaths$hod[deaths$hod == 99] <- NA
deaths$hod[deaths$hod == 24] <- NA

hod <- subset(count(deaths, "hod"),!is.na(hod))
ggplot(hod, aes(x = hod, y = freq)) +
  geom_line()

hod2 <- count(deaths, c("cod", "hod"))
hod2 <- ddply(hod2, "cod", mutate, prop = freq / sum(freq))
# write.csv(hod2,"~/hod2.csv",row.names = FALSE)


overall <- ddply(hod2, "hod", summarise, freq_all = sum(freq))
overall <- mutate(overall, prop_all = freq_all / sum(freq_all))

hod2 <- join(overall, hod2, by = "hod")

devi <- ddply(hod2,
              "cod",
              summarise,
              n = sum(freq),
              dist = mean((prop - prop_all) ^ 2))
devi <- subset(devi, n > 50)

deaths$loc <-
  with(deaths, str_c(statD, countyD, locationD, sep = "-"))

deaths$long <- with(locations, long[match(deaths$loc, id)])
deaths$lat <- with(locations, lat[match(deaths$loc, id)])
deaths["loc"] <- list(NULL)
deaths <- na.omit(deaths)

# Map visualization -------------------------------------------------------

locs <- count(deaths, c("long", "lat"))
ggplot(locs, aes(long, lat)) +
  geom_polygon(aes(group = states$group),
               data = states,
               color = "white",
               fill = "grey90") +
    geom_point(aes(size = freq), alpha = 1 / 5) +
  scale_size_area(breaks = c(1, 100, 500, 1000, 5000, 10000)) +
  coord_map()

# Weather visualization ---------------------------------------------------

deaths_2008 <-
  deaths[which(
    deaths$yod == 2008 & deaths$mod != 0 & deaths$dod != 0),]
deaths_2008  <-
  mutate(deaths_2008, dateod = ymd(str_c(yod, mod, dod, sep = '-')))
daily <- mutate(daily,day=ymd(daily$day))

deaths_2008$disease <- with(codes,disease[match(deaths_2008$cod,code)])
deaths_2008$temp_min <- with(daily,temp_min[match(deaths_2008$dateod,day)])
deaths_2008$temp_max <- with(daily,temp_max[match(deaths_2008$dateod,day)])
deaths_2008$wind <- with(daily,wind[match(deaths_2008$dateod,day)])

deaths_2008 %>% 
  count("temp_min") %>% 
  na.omit() %>% 
  ggplot(., aes(temp_min,freq),) + 
  geom_point()+
  scale_x_continuous()+
  geom_smooth()

deaths_2008 %>% 
  group_by(dateod, temp_min) %>% 
  tally(name = "freq")%>% 
  filter(freq>=1200) %>%
  na.omit() %>%
  ggplot(., aes(temp_min, freq)) +
  geom_point() +
  scale_x_continuous() +
  geom_smooth()




deaths_2008 %>% count("temp_max") %>% na.omit() %>% 
  ggplot(.,aes(temp_max,freq)) + 
  geom_point()+
  scale_x_continuous() +
  geom_smooth()

deaths_2008 %>% 
  group_by(dateod, temp_max) %>% 
  tally(name = "freq")%>% 
  filter(freq>=1200) %>%
  na.omit() %>%
  ggplot(., aes(temp_max, freq)) +
  geom_point() +
  scale_x_continuous() +
  geom_smooth()


deaths_2008 %>% count("wind") %>% na.omit() %>% 
  ggplot(.,aes(wind,freq)) + 
  geom_point()+
  scale_x_continuous() +
  geom_smooth()

deaths_2008 %>% 
  group_by(dateod, wind) %>% 
  tally(name = "freq")%>% 
  filter(freq>=1200) %>% 
  na.omit() %>%
  ggplot(., aes(wind, freq)) +
  geom_point() +
  scale_x_continuous() +
  geom_smooth()

