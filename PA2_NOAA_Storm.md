# IMPACT OF TORNADOS, HEAT ETC. ON PUBLIC HEALTH AND THE US ECONOMY
Which event types (tornados, floods, heat etc.) are most harmful in respect to public health and the US economy?

## SYNOPSIS 
To discuss the above, we use the events from the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage. The dataset used here starts in the year 1950 and ends in November 2011.

The two main questions are: 
1. Across the United States, which types of events are most harmful with respect to population health?
2. Across the United States, which types of events have the greatest economic consequences?

To answer the first question in detail, we encourage you to check HARMFUL EVENTS BY INJURIES, FATALITIES. And following THE IMPACT ON THE ECONOMY you will get a detailed answer to the second.


## DATA PROCESSING
The [Storm Data, Zip, 47Mb](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) , as well as the [National Weather Service Storm Data Documentation, PDF](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) and the [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf) explaining the data, have been downloaded with *download.file*. 

To explore the storm data, we use the programming language R on an Apple computer as can be seen in the Session Info below.
We use read.csv to load the zipped data and create the stormdata data frame. The data frame has 902'297 observations and 37 variables.



```r
## rm(list=ls())
## setwd("/Users/rogerfischer/datasciencecoursera/repdata/RepData_PeerAssessment2")

## Session Info about tools used (OS, R version, R packages, etc.)
sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_1.0.0 knitr_1.9    
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-4 digest_0.6.6     evaluate_0.5.5   formatR_1.0     
##  [5] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  labeling_0.3    
##  [9] markdown_0.7.4   MASS_7.3-35      mime_0.2         munsell_0.4.2   
## [13] plyr_1.8.1       proto_0.3-10     Rcpp_0.11.3      reshape2_1.4.1  
## [17] rmarkdown_0.3.10 scales_0.2.4     stringr_0.6.2    tools_3.1.2
```

```r
## Storm Data, 47Mb, zipped with bzip2 algorithm
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2", method="curl")
## read.csv can read zipped files directly
stormdata <- read.csv("StormData.csv.bz2")

## Early Exploration
dim(stormdata)
```

```
## [1] 902297     37
```

```r
## summary(stormdata$EVTYPE)
summary(stormdata$FATALITIES)
```

```
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
##   0.0000   0.0000   0.0000   0.0168   0.0000 583.0000
```

```r
class(stormdata$FATALITIES)
```

```
## [1] "numeric"
```

```r
sum_fatalities <- sum(stormdata$FATALITIES)
sum_fatalities
```

```
## [1] 15145
```

```r
summary(stormdata$INJURIES)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##    0.0000    0.0000    0.0000    0.1557    0.0000 1700.0000
```

```r
class(stormdata$INJURIES)
```

```
## [1] "numeric"
```

```r
sum_injuries <- sum(stormdata$INJURIES)
sum_injuries 
```

```
## [1] 140528
```


## HARMFUL EVENTS BY INJURIES, FATALITIES
As we want to answer the question: "Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?", we first  create a subset of the data by selecting all events which have fatalities or injuries greater than zero. We already know that the total of all injuries is 140'528 and the total of all fatalities is 15'145.


```r
harmful <- subset(stormdata, stormdata$FATALITIES > 0 & stormdata$INJURIES > 0)
dim(harmful)
```

```
## [1] 2649   37
```

Then we filter out only the three columns needed to answer the question plus the date. Tornado immediately looks like a category with a lot of fatalities and injuries.


```r
harmful_small <- harmful[, c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES")]
summary(harmful_small)
```

```
##               BGN_DATE               EVTYPE       FATALITIES     
##  4/3/1974 0:00:00 :  65   TORNADO       :1378   Min.   :  1.000  
##  4/11/1965 0:00:00:  37   LIGHTNING     : 263   1st Qu.:  1.000  
##  4/27/2011 0:00:00:  33   TSTM WIND     : 148   Median :  1.000  
##  3/21/1952 0:00:00:  21   FLASH FLOOD   :  85   Mean   :  2.862  
##  2/5/2008 0:00:00 :  14   HIGH WIND     :  69   3rd Qu.:  2.000  
##  5/31/1985 0:00:00:  13   EXCESSIVE HEAT:  64   Max.   :158.000  
##  (Other)          :2466   (Other)       : 642                    
##     INJURIES      
##  Min.   :   1.00  
##  1st Qu.:   2.00  
##  Median :   5.00  
##  Mean   :  29.82  
##  3rd Qu.:  20.00  
##  Max.   :1700.00  
## 
```

```r
## Tornado Subset
harmful_tornado <- subset(harmful_small, EVTYPE == "TORNADO")
dim(harmful_tornado)
```

```
## [1] 1378    4
```

```r
summary(harmful_tornado)
```

```
##               BGN_DATE                      EVTYPE       FATALITIES     
##  4/3/1974 0:00:00 :  65   TORNADO              :1378   Min.   :  1.000  
##  4/11/1965 0:00:00:  37      HIGH SURF ADVISORY:   0   1st Qu.:  1.000  
##  4/27/2011 0:00:00:  33    COASTAL FLOOD       :   0   Median :  2.000  
##  3/21/1952 0:00:00:  21    FLASH FLOOD         :   0   Mean   :  3.793  
##  2/5/2008 0:00:00 :  13    LIGHTNING           :   0   3rd Qu.:  3.000  
##  5/31/1985 0:00:00:  13    TSTM WIND           :   0   Max.   :158.000  
##  (Other)          :1196   (Other)              :   0                    
##     INJURIES      
##  Min.   :   1.00  
##  1st Qu.:   5.00  
##  Median :  13.00  
##  Mean   :  43.68  
##  3rd Qu.:  40.00  
##  Max.   :1700.00  
## 
```

```r
tornado_fatalities <- sum(harmful_tornado$FATALITIES)
tornado_fatalities
```

```
## [1] 5227
```

```r
tornado_injuries <- sum(harmful_tornado$INJURIES)
tornado_injuries
```

```
## [1] 60187
```

Tornado is definitely one of the more harmful event types, as evidenced by the fatalities, 5'227 of 15'415 total, and the injuries, 60'187 from total 140'528. Next to that, others will pale in comparison. We will now look at event types with fatalities and injuries higher then the 3rd quartile and higher as the mean, as seen in summary(harmful_small) above. 


```r
# over the 3rd quartile
harmful_4qu <- subset(harmful_small, FATALITIES > 2 & INJURIES > 20)
dim(harmful_4qu)
```

```
## [1] 377   4
```

```r
harmful_evtypes <- unique(harmful_4qu$EVTYPE)
harmful_evtypes
```

```
##  [1] TORNADO               WILD FIRES            TROPICAL STORM GORDON
##  [4] BLIZZARD              WATERSPOUT/TORNADO    EXCESSIVE HEAT       
##  [7] HEAT                  HEAT WAVE             EXTREME COLD         
## [10] HIGH WIND             WINTER STORM          ICE STORM            
## [13] FOG                   FLASH FLOOD           FLOOD                
## [16] WILDFIRE              HURRICANE/TYPHOON     TSUNAMI              
## [19] THUNDERSTORM WIND    
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

```r
# over the mean, as a cross check
harmful_mean <- subset(harmful_small, FATALITIES > 2.862 & INJURIES > 29.82)
dim(harmful_mean)
```

```
## [1] 338   4
```

```r
harmful_evtypes2 <- unique(harmful_mean$EVTYPE)
harmful_evtypes2
```

```
##  [1] TORNADO               WILD FIRES            TROPICAL STORM GORDON
##  [4] BLIZZARD              WATERSPOUT/TORNADO    EXCESSIVE HEAT       
##  [7] HEAT                  HEAT WAVE             EXTREME COLD         
## [10] HIGH WIND             WINTER STORM          ICE STORM            
## [13] FLASH FLOOD           FLOOD                 FOG                  
## [16] WILDFIRE              HURRICANE/TYPHOON     TSUNAMI              
## [19] THUNDERSTORM WIND    
## 985 Levels:    HIGH SURF ADVISORY  COASTAL FLOOD ... WND
```

We know have 19 event types that remain. Plotting will help us to get a better picture.


```r
library(ggplot2)
qplot(x = FATALITIES, y = INJURIES, data = harmful_4qu, facets = EVTYPE ~ . ,  color = EVTYPE, geom =   c("point"))
```

![plot of chunk most_dangerous](figure/most_dangerous-1.png) 

The 3 most harmful events types are:   
- Tornados    
- Excessive Heat    
- Heat 


```r
most_harmful <- subset(harmful_small, EVTYPE == "TORNADO" | EVTYPE == "EXCESSIVE HEAT" | EVTYPE == "HEAT")
qplot(x = FATALITIES, y = INJURIES, data = most_harmful, facets = EVTYPE ~ . ,  color = EVTYPE, geom =   c("point"))
```

![plot of chunk tornado_heat](figure/tornado_heat-1.png) 


## THE IMPACT ON THE ECONOMY
To be able to discuss the economic impact of different event types, we first need to find the damage variables. Here they are:   

Property Damage Variables  
- PROPDMG   
- PROPMGEXP 

Crop Damage Variables  
- CROPDMG   
- CROPDMGEXP

EXP = Exponent
- 10.00K = $10'000    
- 10.00M = $10'000'000     
- 10.00B = $10'000'000'000 ???, this is to prove    

We take damage in the billions as our measure to qualify the most economically harmful events. There are obviously other ways (for example long tail cumulated damages).


```r
table(stormdata$PROPDMGEXP)
```

```
## 
##             -      ?      +      0      1      2      3      4      5 
## 465934      1      8      5    216     25     13      4      4     28 
##      6      7      8      B      h      H      K      m      M 
##      4      5      1     40      1      6 424665      7  11330
```

```r
table(stormdata$CROPDMGEXP)
```

```
## 
##             ?      0      2      B      k      K      m      M 
## 618413      7     19      1      9     21 281832      1   1994
```

```r
b_damage <- subset(stormdata, PROPDMGEXP == "B" | CROPDMGEXP == "B")
dim(b_damage)
```

```
## [1] 47 37
```

```r
## B is definitely Billion
# b_damage[1, c(2, 8, 23:28, 36) ] # Estimates between $50 and $100 million 
# b_damage[3, c(2, 8, 23:28, 36) ] # $400 Million Crop Damage
# b_damage[4, c(2, 8, 23:28, 36) ] # $2.1 billion dollars
```

B in the exponent variables (PROPDMGEXP, CROPDMGEXP) means billion, but as we will see below, this is not always true. High numbers over hundred do definitely not correspond to billions.


```r
dmgB <- subset(stormdata, stormdata$PROPDMGEXP == "B" & stormdata$PROPDMG > 0 | stormdata$CROPDMGEXP == "B" & stormdata$CROPDMG > 0)
dim(dmgB)
```

```
## [1] 45 37
```

```r
dmgB_small <- dmgB[ , c(2, 8, 23:28)]
summary(dmgB_small)
```

```
##               BGN_DATE                EVTYPE     FATALITIES     
##  8/29/2005 0:00:00: 3   HURRICANE/TYPHOON:12   Min.   :  0.000  
##  1/1/2006 0:00:00 : 2   FLOOD            : 5   1st Qu.:  0.000  
##  10/4/1995 0:00:00: 2   HURRICANE        : 3   Median :  0.000  
##  3/12/1993 0:00:00: 2   TORNADO          : 3   Mean   :  7.444  
##  4/27/2011 0:00:00: 2   DROUGHT          : 2   3rd Qu.:  4.000  
##  5/1/2011 0:00:00 : 2   HURRICANE OPAL   : 2   Max.   :158.000  
##  (Other)          :32   (Other)          :18                    
##     INJURIES       PROPDMG         PROPDMGEXP    CROPDMG      
##  Min.   :   0   Min.   :  0.00   B      :40   Min.   :  0.00  
##  1st Qu.:   0   1st Qu.:  1.04          : 4   1st Qu.:  0.00  
##  Median :   0   Median :  2.09   K      : 1   Median :  0.00  
##  Mean   :  66   Mean   : 17.24   -      : 0   Mean   : 35.56  
##  3rd Qu.:   0   3rd Qu.:  5.00   ?      : 0   3rd Qu.:  5.00  
##  Max.   :1150   Max.   :500.00   +      : 0   Max.   :500.00  
##                                  (Other): 0                   
##    CROPDMGEXP
##         :16  
##  K      :11  
##  M      :11  
##  B      : 7  
##  ?      : 0  
##  0      : 0  
##  (Other): 0
```

```r
dmgB_small
```

```
##                  BGN_DATE                     EVTYPE FATALITIES INJURIES
## 187564  3/12/1993 0:00:00               WINTER STORM          4        0
## 187566  10/4/1995 0:00:00  HURRICANE OPAL/HIGH WINDS          2        0
## 188633  8/20/1995 0:00:00                       HEAT          0        0
## 195000  10/3/1995 0:00:00             HURRICANE OPAL          1        0
## 195001  3/12/1993 0:00:00 TORNADOES, TSTM WIND, HAIL         25        0
## 195007  10/4/1995 0:00:00             HURRICANE OPAL          0        0
## 198389  8/31/1993 0:00:00                RIVER FLOOD          0        0
## 199733   8/1/1995 0:00:00                    DROUGHT          0        0
## 201256  9/21/1995 0:00:00                     FREEZE          0        0
## 207175   5/8/1995 0:00:00  HEAVY RAIN/SEVERE WEATHER          0        0
## 211900   2/9/1994 0:00:00                  ICE STORM          0        0
## 243445   5/5/1995 0:00:00        SEVERE THUNDERSTORM          0        0
## 298088  4/18/1997 0:00:00                      FLOOD          0        0
## 347872  9/21/1998 0:00:00                  HURRICANE          0        0
## 366694  9/15/1999 0:00:00                  HURRICANE          0        0
## 397334   5/4/2000 0:00:00           WILD/FOREST FIRE          0        0
## 443782   6/5/2001 0:00:00             TROPICAL STORM         22        0
## 485577   5/7/2003 0:00:00                FLASH FLOOD          0        0
## 488045 10/25/2003 0:00:00                   WILDFIRE         14       90
## 525145  9/13/2004 0:00:00          HURRICANE/TYPHOON          0        0
## 529351  8/13/2004 0:00:00          HURRICANE/TYPHOON          7      780
## 529363  8/13/2004 0:00:00                  HIGH WIND          4        0
## 529436   9/4/2004 0:00:00          HURRICANE/TYPHOON          0        0
## 529498  9/13/2004 0:00:00          HURRICANE/TYPHOON          7        0
## 565003  8/27/2005 0:00:00          HURRICANE/TYPHOON          0        0
## 569065   7/9/2005 0:00:00          HURRICANE/TYPHOON          0        0
## 569308 10/24/2005 0:00:00          HURRICANE/TYPHOON          5        0
## 577675  8/28/2005 0:00:00          HURRICANE/TYPHOON          0        0
## 577676  8/29/2005 0:00:00                STORM SURGE          0        0
## 577683  9/23/2005 0:00:00          HURRICANE/TYPHOON          1        0
## 581533  8/28/2005 0:00:00          HURRICANE/TYPHOON          0        0
## 581535  8/29/2005 0:00:00                STORM SURGE          0        0
## 581537  8/29/2005 0:00:00          HURRICANE/TYPHOON         15      104
## 598502  9/23/2005 0:00:00          HURRICANE/TYPHOON          1        0
## 605953   1/1/2006 0:00:00                      FLOOD          0        0
## 639347   1/1/2006 0:00:00                    DROUGHT          0        0
## 739575  9/12/2008 0:00:00                  HURRICANE          0        0
## 739576  9/12/2008 0:00:00           STORM SURGE/TIDE         11        0
## 808257   5/1/2010 0:00:00                      FLOOD         10        0
## 834674  10/5/2010 0:00:00                       HAIL          0        1
## 856214  4/27/2011 0:00:00                    TORNADO          4       45
## 860386  4/27/2011 0:00:00                    TORNADO         44      800
## 862634  5/22/2011 0:00:00                    TORNADO        158     1150
## 867749   5/1/2011 0:00:00                      FLOOD          0        0
## 868046   5/1/2011 0:00:00                      FLOOD          0        0
##        PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP
## 187564    5.00          B    0.00           
## 187566    0.10          B   10.00          M
## 188633    0.00               0.40          B
## 195000    2.10          B    5.00          M
## 195001    1.60          B    2.50          M
## 195007    1.00          B    0.00           
## 198389    5.00          B    5.00          B
## 199733    0.00               0.50          B
## 201256    0.00               0.20          B
## 207175    2.50          B    0.00           
## 211900  500.00          K    5.00          B
## 243445    1.20          B    0.00           
## 298088    3.00          B    0.00           
## 347872    1.70          B  301.00          M
## 366694    3.00          B  500.00          M
## 397334    1.50          B    0.00           
## 443782    5.15          B    0.00           
## 485577    1.00          B    0.00          K
## 488045    1.04          B    6.50          M
## 525145    2.50          B   25.00          M
## 529351    5.42          B  285.00          M
## 529363    1.30          B    0.00           
## 529436    4.83          B   93.20          M
## 529498    4.00          B   25.00          M
## 565003    1.00          B    0.00           
## 569065    1.50          B  300.00          K
## 569308   10.00          B    0.00           
## 577675   16.93          B    0.00           
## 577676   31.30          B    0.00           
## 577683    4.00          B    0.00           
## 581533    7.35          B    0.00           
## 581535   11.26          B    0.00           
## 581537    5.88          B    1.51          B
## 598502    2.09          B    0.00           
## 605953  115.00          B   32.50          M
## 639347    0.00               1.00          B
## 739575    1.00          B    0.00          K
## 739576    4.00          B    0.00          K
## 808257    1.50          B    1.00          K
## 834674    1.80          B    0.00          K
## 856214    1.00          B    0.00          K
## 860386    1.50          B    0.00          K
## 862634    2.80          B    0.00          K
## 867749    1.00          B    0.00          K
## 868046    2.00          B    0.00          K
```

```r
## Checking for outliers

# Checking flood with 115 B
# flood <- subset(dmgB, dmgB$EVTYPE == "FLOOD")
# flood[ 2, c(2, 8, 23:28, 36)] # is a wrong outlier, $6 Mio, $70 Mio vs $115 Billion !!!

# Checking Ice Storm with 5 B
# icestorm <- subset(dmgB, dmgB$EVTYPE == "ICE STORM")
# icestorm[ , c(2, 8, 23:28, 36)] # it's $500 Mio damage to utilities, not $5.5 Million crop damage by year/over 10 years

# Getting a visual feel of the frequency of the billion dollar events
# qplot(x = BGN_DATE, y = CROPDMG, data = dmgB_small, facets = EVTYPE ~ . ,  color = EVTYPE, geom =   c("point"))
# qplot(x = BGN_DATE, y = PROPDMG, data = dmgB_small, facets = EVTYPE ~ . ,  color = EVTYPE, geom =   c("point"))
```
    
     
Looking at the data of dmgB_small and the summary of dmgB_small, Hurricanes, Hurricane/Typhons, Tropical Storms are the most frequent billion dollar damage causing event types. Next to that, tornados and floods can also have billion dollar huge economic impacts. 

## RESULTS
Our results for the two questions:   
Across the United States, which types of events are most harmful with respect to population health?
In regard to population health (fatalities and injuries combined) we found that the theses 3 event types are the most harmful     
* Tornados  
* Excessive Heat
* Heat
   
Across the United States, which types of events have the greatest economic consequences?     
In regard to economic consequences we found that these 3 event types have the highest economic impact:    
* Hurricanes and Typhons 
* Tornados
* Floods  

Hurricanes and Typhons are also the most frequent and so their economic impact is the highest by event but also overall. Tornados in 2011 also had a high economic impact next to causing a lot of fatalities and injuries. In comparison to the first question, it is however much more difficult to foresee future events. Several event types can cause major economic damage at any moment. 

The increase in billion dollar damage events is also quite striking, all "B" events happened between 1993 and 2011. Looking at this data, it is relatively easy to predict that billion dollar damage events will be with us for the foreseeable future.



