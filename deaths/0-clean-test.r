pacman::p_load(foreign, stringr, ggplot2, plyr,dplyr)
rm(list = ls(all.names = TRUE))
gc(full = TRUE)
options(stringsAsFactors = FALSE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

deaths <- read.csv("./deaths08.csv.bz2", header = TRUE)
codes <- read.csv("../disease/icd-main.csv", header = TRUE)
locations <- read.csv("../locations/locations.csv.bz2",header = TRUE)

cause <- arrange(count(deaths, "cod"), desc("freq"))

names(cause)[1] <- "code"
cause <- join(cause, codes)
# attach(cause)
# ggplot(cause,aes(x=freq,y=disease)) +
#               geom_point()+
#         scale_x_log10()

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

deaths$loc <- with(deaths, str_c(statD, countyD, locationD, sep = "-"))

deaths$long <- with(locations,long[match(deaths$loc,id)])

deaths$lat <- with(locations,lat[match(deaths$loc,id)])

deaths["loc"] <- list(NULL)

deaths <- na.omit(deaths)


locs <- count(deaths,c("long","lat"))
ggplot(locs,aes(long,lat)) +
  geom_polygon(aes(group=group), data=states,
               color="white",fill="grey90") +
  geom_point(aes(size=freq,order=freq),
             alpha=1/3, to=c(0.1,6))+
  scale_area(breaks=c(1,100,500,1000,5000,10000),
             to = c(0.5,10))+
  coord_map()
