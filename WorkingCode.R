require(ggplot2)
require(lubridate)
require(plyr)
require(gridExtra)
require(scales)

## Read the storm data
storm.data <- read.table(
  bzfile(
    description = "repdata-data-StormData.csv.bz2", 
    open = "r"),
  header = TRUE,
  sep = ",")

######################################################
## Begin Tidy Data
######################################################

## Subset to include on the needed fields
## (this should speed up analysis)
working.storm.data <- storm.data[, c(
  "EVTYPE", 
  "BGN_DATE",
  "FATALITIES", 
  "INJURIES", 
  "PROPDMG", 
  "PROPDMGEXP", 
  "CROPDMG", 
  "CROPDMGEXP")]

## Add POSIX formatted date
working.storm.data$EVENT_DATE <- as.POSIXct(
  working.storm.data$BGN_DATE, 
  format = "%m/%d/%Y %H:%M:%S")

## Convert property damage and crop damage units to uppercase
working.storm.data$PROPDMGEXP <- factor(toupper(working.storm.data$PROPDMGEXP))
working.storm.data$CROPDMGEXP <- factor(toupper(working.storm.data$CROPDMGEXP))

## List of valid exponent codes
validExponents <- c("H", "K", "M", "C")

## Expand property damage values
working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "H")] <- 
  working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "H")] * 10^2
working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "K")] <- 
  working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "K")] * 10^3
working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "M")] <- 
  working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "M")] * 10^6
working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "B")] <- 
  working.storm.data$PROPDMG[(working.storm.data$PROPDMGEXP == "B")] * 10^9
working.storm.data$PROPDMG[!(working.storm.data$PROPDMGEXP %in% validExponents)] <- 0

## Expand crop damage values
working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "H")] <- 
  working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "H")] * 10^2
working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "K")] <- 
  working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "K")] * 10^3
working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "M")] <- 
  working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "M")] * 10^6
working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "B")] <- 
  working.storm.data$CROPDMG[(working.storm.data$CROPDMGEXP == "B")] * 10^9
working.storm.data$CROPDMG[!(working.storm.data$CROPDMGEXP %in% validExponents)] <- 0

## Drop unecessary exponent fields
working.storm.data <- working.storm.data[, !(names(working.storm.data) %in% c(
  "BGN_DATE", 
  "PROPDMGEXP", 
  "CROPDMGEXP"))]

## Rename existing fields
names(working.storm.data) <- c(
  "event.type",
  "fatalities", 
  "injuries", 
  "property.damage", 
  "crop.damage",
  "event.date")

######################################################
## End Tidy Data
######################################################

######################################################
## Top 10 Health Effects
######################################################

## Aggregate and order fatality data
fatalities <- aggregate(fatalities ~ event.type, data = working.storm.data, FUN = sum)
fatalities <- fatalities[order(-fatalities$fatalities),][1:10,]
fatalities$event.type <- factor(fatalities$event.type, levels = fatalities$event.type)

## Create fatalities plot
fatalities.plot <- ggplot(fatalities, aes(
    x = event.type, 
    y = fatalities, 
    label = fatalities, 
    fill = fatalities)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Events Responsible for Fatalities", 
       x = "Event Type", 
       y = "Number of Fatalities") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

## Aggregate and order injuries data
injuries <- aggregate(injuries ~ event.type, data = working.storm.data, FUN = sum)
injuries <- injuries[order(-injuries$injuries),][1:10,]
injuries$event.type <- factor(injuries$event.type, levels = injuries$event.type)

## Create injuries plot
injuries.plot <- ggplot(injuries, aes(x = event.type, y = injuries, fill = injuries)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Events Responsible for Injuries", 
       x = "Event Type", 
       y = "Number of Injuries") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

## Combine the fatalities and injuries plots into the same figure
grid.arrange(
  fatalities.plot, 
  injuries.plot, 
  ncol = 2, 
  sub = textGrob(
    "Figure 1 - Top Events Contributing to Injuries/Fatalities", 
    gp = gpar(fontface = "bold", col = "grey20", fontsize = 12)))

######################################################
## Top Fatality Trend
######################################################

## Extract tornado data
tornado.data <- working.storm.data[(working.storm.data$event.type == "TORNADO"),]

## Round to year level
tornado.data$event.year <- floor_date(tornado.data$event.date, "year")

## Aggregate fatalities and injuries per year
tornado.health.effects.by.year <- ddply(tornado.data, "event.year", summarise, health.effects = sum(fatalities, injuries))

## Create yearly fatality/injury plot
health.effects.trend.plot <- 
  ggplot(
    data = tornado.health.effects.by.year, 
    aes(x = event.year, y = health.effects)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Inclimate Weather Health Effects by Year", 
       x = "Event Year", 
       y = "Number of Fatalities/Injuries") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

## Display health effects plot with sub-heading
grid.arrange(health.effects.trend.plot,
             sub = textGrob(
               "Figure 2 - Health Effects Trend by Year", 
               gp = gpar(fontface = "bold", col = "grey20", fontsize = 12)))

######################################################
## Top 10 Economic Effects
######################################################

## Aggregate and order property damage data
property.damage <- aggregate(property.damage ~ event.type, data = working.storm.data, FUN = sum)
property.damage <- property.damage[order(-property.damage$property.damage),][1:10,]
property.damage$event.type <- factor(property.damage$event.type, levels = property.damage$event.type)

## Create property damage plot
property.damage.plot <- ggplot(property.damage, aes(
    x = event.type, 
    y = property.damage, 
    label = property.damage, 
    fill = property.damage
  )) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Events Responsible for Property Damage", 
       x = "Event Type", 
       y = "Cost of Property Damage") +
  scale_y_continuous(
    labels = function(x) paste0(
      "$", 
      format(x/10^9, big.mark = ",", scientific = FALSE),
      "B")) +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

## Aggregate and order crop damage data
crop.damage <- aggregate(crop.damage ~ event.type, data = working.storm.data, FUN = sum)
crop.damage <- crop.damage[order(-crop.damage$crop.damage),][1:10,]
crop.damage$event.type <- factor(crop.damage$event.type, levels = crop.damage$event.type)

## Create property damage plot
crop.damage.plot <- ggplot(crop.damage, aes(
  x = event.type, 
  y = crop.damage, 
  label = crop.damage, 
  fill = crop.damage
)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Events Responsible for Crop Damage", 
       x = "Event Type", 
       y = "Cost of Crop Damage") +
  scale_y_continuous(
    labels = function(x) paste0(
      "$", 
      format(x/10^9, big.mark = ",", scientific = FALSE),
      "B")) +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank())

## Combine the property and crop damage plots into the same figure
grid.arrange(
  property.damage.plot, 
  crop.damage.plot, 
  ncol = 2, 
  sub = textGrob(
    "Figure 3 - Top Events Contributing to Property/Crop Damage", 
    gp = gpar(fontface = "bold", col = "grey20", fontsize = 12)))