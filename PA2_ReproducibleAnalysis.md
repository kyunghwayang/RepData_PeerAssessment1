---
title: "The effects of harmful weather events on health and economic consequences in the US"
author: K. Yang
output: 
  html_document:
    keep_md: true
---



### Executive Summary

Among all the weather events, tornado has left the most significant impact on both public health and economic consequences. Public health was measured in terms of fatalities and injuries related to each event. Economic consequences were measured in terms of crop and property damages. While hail had the most significant impact on crop damage, overall, tornado has caused the highest property damage and overall economic damage. 
  

### Data Processing

**1. Create a directory for the data and download the storm data** 
Because bz2 does not require unzip, it was read directly and loaded into the work space. 

```r
if(!file.exists("data")) dir.create("data")

## - .z - .gz - .bz2: do not need unzip.
url<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "./data/stormData")
data <- read.csv("./data/stormData")
```

**2. Exploratory Data Analysis**  
- What are the event types?  
- What is the relationship between each event type and public health?  
- What is the relationship between each event type and economic consequences? 
Note: Missing data is ignored in this report. 

2.1 Events and public health  

Public health was measured in terms of fatalities and injuries.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

dim(data)
```

```
## [1] 902297     37
```

```r
names(data)
```

```
##  [1] "STATE__"    "BGN_DATE"   "BGN_TIME"   "TIME_ZONE"  "COUNTY"    
##  [6] "COUNTYNAME" "STATE"      "EVTYPE"     "BGN_RANGE"  "BGN_AZI"   
## [11] "BGN_LOCATI" "END_DATE"   "END_TIME"   "COUNTY_END" "COUNTYENDN"
## [16] "END_RANGE"  "END_AZI"    "END_LOCATI" "LENGTH"     "WIDTH"     
## [21] "F"          "MAG"        "FATALITIES" "INJURIES"   "PROPDMG"   
## [26] "PROPDMGEXP" "CROPDMG"    "CROPDMGEXP" "WFO"        "STATEOFFIC"
## [31] "ZONENAMES"  "LATITUDE"   "LONGITUDE"  "LATITUDE_E" "LONGITUDE_"
## [36] "REMARKS"    "REFNUM"
```

```r
# summarise the fatalities and injuries
health <- data %>%
        group_by (EVTYPE) %>%
        summarise(fatalities=sum(FATALITIES, na.rm=TRUE), injuries=sum(INJURIES, na.rm=TRUE)) %>%
        arrange(desc(fatalities, injuries))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# extract top 10        
health <- health[1:10,]

# process the top 10 data for a bar plot, fatalities and injuries side by side.
df <- data.frame(event = rep(health$EVTYPE, 2), 
                     total=c(health$fatalities, health$injuries),
                     type = factor(rep(c("fatalities", "injuries"), each=nrow(health))))

g<- ggplot(df, aes(x=event, y=total)) +
  facet_grid(type~.)+
  geom_col() +
  theme(axis.text.x = element_text(size=5))
g
```

![](PA2_ReproducibleAnalysis_files/figure-html/health-1.png)<!-- -->

2.2 Economic consequences

This impact was measured in terms of crop damage and property damage. 


```r
# extract event type, crop damage, and property damage
dmg <- data %>%
        group_by (EVTYPE) %>%
        summarise(crop=sum(CROPDMG, na.rm=TRUE), prop=sum(PROPDMG, na.rm=TRUE)) %>%
        mutate(total=crop+prop) %>%
        arrange(desc(total))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# extract top 10        
dmg<- dmg[1:10,]


# process the top 10 data for a bar plot, crop and property damage side by side.
type <- gl(2, nrow(dmg), labels = c("crop", "property"))
df2 <- data.frame(event = rep(dmg$EVTYPE, 2), 
                dmg=c(dmg$crop, dmg$prop),
                type = type)

g2<- ggplot(df2, aes(x=event, y=dmg)) +
  facet_grid(type~.)+
  geom_col() +
  labs(y="damage") +
  theme(axis.text.x = element_text(size=5))
g2
```

![](PA2_ReproducibleAnalysis_files/figure-html/damages-1.png)<!-- -->


### Results  

**1. Impact on public health**


```r
torn <-df %>% filter(event=="TORNADO")
```

As shown in the plot, both fatalities and injuries were highest when tornado occurred. The total number of fatalities related to tornado was 5633 and that of injuries was 91346.

**2. Economic consequences**

```r
torn_econ <-df2 %>% filter(event=="TORNADO")
torn_crop <- prettyNum(torn_econ[1, 2], big.mark = ",", scientific = FALSE)  
torn_prop <- prettyNum(torn_econ[2, 2], big.mark = ",", scientific = FALSE)
hail_econ <-df2 %>% filter(event=="HAIL")
hail_crop <- prettyNum(hail_econ[1, 2], big.mark = ",", scientific = FALSE)
hail_prop <- prettyNum(hail_econ[2, 2], big.mark = ",", scientific = FALSE)
```

As shown in the second plot, the economic impact on crop and property damages was also the highest when tornado occurred. The damage on property was exceptionally high, 3,212,258 dollars. Among all the events, hail left the highest damage on crop, 579,596.3dollars.

