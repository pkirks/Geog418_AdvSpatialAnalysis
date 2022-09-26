#######
## For this lab you will need the following libraries:
## terra, lubridate, e1071, gridExtra, gtable, grid, ggplot2, dplyr, bcmaps, tmap, and sf 
#####
#Install Libraries
#install.packages("raster")
#install.packages("Rcpp")

install.packages("terra")
install.packages("lubridate")
install.packages("e1071")
install.packages("gridExtra")
install.packages("gtable")
install.packages("grid")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("bcmaps")
install.packages("tmap")
install.packages("sf")


#####
#Load Libraries
library("terra")
library("lubridate")
library("e1071")
library("gridExtra")
library("gtable")
library("grid")
library("ggplot2")
library("dplyr")
library("bcmaps")
library("tmap")
library("sf")


#####
#Set working directory
dir <- "C:/Users/philk/Desktop/Spring 2022/Geog 418/Assignment1_v2/PROT_HISTORICAL_INCIDENTS_SP"
setwd(dir)
getwd()

#####
#Read in data and extract attribute table
shp <- vect("./H_FIRE_PNT_point.shp") #read in shp file from current (".") working directory
shp

bc <- vect(bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province
shp <- shp[bc] #Clip points to only be inside the BC bounds

df <- as.data.frame(shp) #Extract the attribute table as a dataframe, store in variable
class(df) #ensure new variable is a dataframe

#####
#Inspect data
names(df) #see column names
head(df) #see first 6 rows of data

typeof(df$FIRE_YEAR) #what is the data type of year?
range(df$FIRE_YEAR) #How many years of data is there?

#We will need to subset the data into an given year
df <- subset(df, df$FIRE_YEAR == 1995)


#We will also need to know the date and the month of each fire
df$IGN_DATE <- as.Date(df$IGN_DATE, "%Y%m%d") #Convert to date string
df$IGN_Day <- yday(df$IGN_DATE) #Make new column with day of year for the ignition date
df$IGN_Month <- month(df$IGN_DATE, label = TRUE, abbr = TRUE) #create new column with month of ignition

#Check the range of day and month
range(df$IGN_Day) #Everything seem ok?
unique(df$IGN_Month) #Any months seem odd?

#Remove values with an NA date
df <- subset(df, !is.na(df$IGN_DATE))

#Check the range of IGN Date
range(df$IGN_DATE) #Anything odd?

#Calculate new year column from date and check range
df$IGN_Year <- year(df$IGN_DATE)
range(df$IGN_Year)



#####
#Clean data to appropriate year and calculate descriptive statistics
df_1995 <- df[which(df$IGN_Year == 1995),] #subset only your year

minSize <- min(df_1995$SIZE_HA)
minSize

df_1995 <- subset(df_1995, df_1995$SIZE_HA > 0)
minSize <- min(df_1995$SIZE_HA)
minSize

#Mean
meanPop <- mean(df_1995$SIZE_HA) #This could produce a wrong value (NA) due to a single NA value in data
meanPop <- mean(df_1995$SIZE_HA, na.rm = TRUE) #Use na.rm = TRUE to ignore NA values in calculation

#Check this website for the correct day numbers of your year: https://miniwebtool.com/day-of-the-year-calculator/
meanSummer <- mean(subset(df_1995, IGN_Day >= 182 & IGN_Day <= 243)$SIZE_HA) #Calculate the mean fire size between July 1 and Aug 31

#Standard Deviation
sdPop <- sd(df_1995$SIZE_HA, na.rm = TRUE) #Calculate the SD, ignoring NA values
sdSummer <- sd(subset(df_1995, IGN_Day >= 182 & IGN_Day <= 243)$SIZE_HA) #Calculate the SD, ignoring NA values only for the summer months

#Mode
modePop <- as.numeric(names(sort(table(df_1995$SIZE_HA), decreasing = TRUE))[1]) #make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)

#It will be cleaner if we use a new variable for the summer data
df_Summer <- subset(df_1995, IGN_Day >= 182 & IGN_Day <= 243) #Make new variable to hold summer data
#make frequency table of fire size variable and sort it in desending order and extract the first row (Most Frequent)
modeSummer <- as.numeric(names(sort(table(df_Summer$SIZE_HA), decreasing = TRUE))[1])

#Median
medPop <- median(df_1995$SIZE_HA, na.rm = TRUE)
medSummer <- median(df_Summer$SIZE_HA, na.rm = TRUE)


#Skewness
skewPop <- skewness(df_1995$SIZE_HA, na.rm = TRUE)[1]
skewSummer <- skewness(df_Summer$SIZE_HA, na.rm = TRUE)[1]

#Kurtosis
kurtPop <- kurtosis(df_1995$SIZE_HA, na.rm = TRUE)[1]
kurtSummer <- kurtosis(df_Summer$SIZE_HA, na.rm = TRUE)[1]

#CoV
CoVPop <- (sdPop / meanPop) * 100
CoVSummer <- (sdSummer / meanSummer) * 100

#Normal distribution test
normPop_PVAL <- shapiro.test(df_1995$SIZE_HA)$p.value
normSummer_PVAL <- shapiro.test(df_Summer$SIZE_HA)$p.value

#####
#Create a table of descriptive stats

samples = c("Population", "Summer") #Create an object for the labels
means = c(meanPop, meanSummer) #Create an object for the means
sd = c(sdPop, sdSummer) #Create an object for the standard deviations
median = c(medPop, medSummer) #Create an object for the medians
mode <- c(modePop, modeSummer) #Create an object for the modes
skewness <- c(skewPop, skewSummer) #Create an object for the skewness
kurtosis <- c(kurtPop, kurtSummer) #Create an object for the kurtosis
CoV <- c(CoVPop, CoVSummer) #Create an object for the CoV
normality <- c(normPop_PVAL, normSummer_PVAL) #Create an object for the normality PVALUE

##Check table values for sigfigs?
means= format(round(means, digits = 3), nsmall =3)
sd= format(round(sd, digits =3), nsmall =3)
median= format(round(median, digits =3), nsmall=3)
mode= format(round(mode,digits = 3), nsmall =3)
skewness=round(skewness,3)
kurtosis= round(kurtosis,3)
CoV = round(CoV,3)
normality=signif(normality, 4)



data.for.table1 = data.frame(samples, means, median, mode, sd)  
data.for.table2 = data.frame(samples,skewness, kurtosis, CoV, normality)

#Make table 1
table1 <- tableGrob(data.for.table1, rows = c("","")) #make a table "Graphical Object" (GrOb) 
t1Caption <- textGrob("Table 1: BC 1995 Wildfire Basic Descriptive Statistics 
                      (in hectares)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table1 <- gtable_add_rows(table1, 
                          heights = grobHeight(t1Caption) + padding, 
                          pos = 0)

table1 <- gtable_add_grob(table1,
                          t1Caption, t = 1, l = 2, r = ncol(data.for.table1) + 1)


table2 <- tableGrob(data.for.table2, rows = c("",""))
t2Caption <- textGrob("Table 2: BC 1995 Wildfire Descriptive Statistics distribution 
                      characteristics and variation 
                      (in hectares)", gp = gpar(fontsize = 09))
padding <- unit(5, "mm")

table2 <- gtable_add_rows(table2, 
                          heights = grobHeight(t2Caption) + padding, 
                          pos = 0)

table2 <- gtable_add_grob(table2,
                          t2Caption, t = 1, l = 2, r = ncol(data.for.table2) + 1)



grid.arrange(table1, newpage = TRUE)
grid.arrange(table2, newpage = TRUE)

#Printing a table (You can use the same setup for printing other types of objects (see ?png))
png("Output_Table1.png") #Create an object to print the table to
grid.arrange(table1, newpage = TRUE)
dev.off() #this prints & table close and save

png("Output_Table2.png") #Create an object to print the table to
grid.arrange(table2, newpage = TRUE) #Create table
dev.off()

#####
#Create and Print a histogram
png("Output_Histogram.png")
hist(df_1995$SIZE_HA, breaks = 15, main = "BC 1995 Wild-Fire Histogram", xlab = "Fire Size (ha)") #Base R style
dev.off()

#LOOK FOR AND CORRECT AN ERROR IN THE CODE BELOW
histogram <- ggplot(df_1995, aes(x = SIZE_HA)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Histogram of BC 1995 Wild-Fires", x = "Fire Size (ha)", y = "Frequency", caption = "Figure 1: 1995 Graph of Wild-Fire occurrences in British Columbia") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 700, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100

png("Output_Histogram_annual_ggplot.png")
histogram
dev.off()

histogram <- ggplot(df_Summer, aes(x = SIZE_HA)) + #Create new GGplot object with data attached and fire size mapped to X axis
  geom_histogram(bins = 30, color = "black", fill = "white") + #make histogram with 30 bins, black outline, white fill
  labs(title = "Histogram of BC 1995 Summer Wild-Fires", x = "Fire Size (ha)", y = "Frequency", caption = "Figure 2: Summer 1995 Graph of Wild-Fire occurrences in British Columbia") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) + #set title to center and bold
  scale_y_continuous(breaks = seq(0, 700, by = 100)) # set y axis labels to 0 - 700 incrimenting by 100

png("Output_Histogram_summer_ggplot.png")
histogram
dev.off()



#####
#Creating bar graph
#LOOK FOR AND CORRECT AN ERROR IN THE CODE BELOW
sumMar = sum(subset(df_1995, IGN_Month == "Mar")$SIZE_HA, na.rm = TRUE) #create new object for March
sumApr = sum(subset(df_1995, IGN_Month == "Apr")$SIZE_HA, na.rm = TRUE) #create new object for April
sumMay = sum(subset(df_1995, IGN_Month == "May")$SIZE_HA, na.rm = TRUE) #create new object for May
sumJun = sum(subset(df_1995, IGN_Month == "Jun")$SIZE_HA, na.rm = TRUE) #create new object for June
sumJul = sum(subset(df_1995, IGN_Month == "Jul")$SIZE_HA, na.rm = TRUE) #create new object for July
sumAug = sum(subset(df_1995, IGN_Month == "Aug")$SIZE_HA, na.rm = TRUE) #create new object for August
sumSep = sum(subset(df_1995, IGN_Month == "Sep")$SIZE_HA, na.rm = TRUE) #create new object for September
months = c("Mar","Apr","May","Jun","Jul", "Aug", "Sep")  #Create labels for the bar graph

png("Output_BarGraph.png") #Create an object to print the bar graph 
barplot(c(sumMar,sumApr,sumMay, sumJun, sumJul, sumAug, sumSep), names.arg = months, 
        main = "Sum of Fire Size by Month", ylab = "Fire Size Sum (ha)", xlab = "Month") #Create the bar graph
dev.off() #Print bar graph

#Total Size by Month GGPLOT
#LOOK FOR AND CORRECT ERRORS IN THE CODE BELOW
barGraph <- df_1995 %>% #store graph in bargraph variable and pass data frame as first argument in next line
  group_by(IGN_Month) %>% #use data frame and group by month and pass to first argument in next line
  summarise(sumSize = sum(SIZE_HA, na.rm = TRUE)) %>% #sum up the total fire size for each month and pass to GGplot
  ggplot(aes(x = IGN_Month, y = sumSize)) + #make new GGPLOT with summary as data and month and total fire size as x and y
  geom_bar(stat = "identity") + #make bar chart with the Y values from the data (identity)
  labs(title = "1995 BC Wild-fire Size Summation by Month", x = "Month", y = "Fire Size Sum (ha)", caption = "Figure 3: British Columbia wild-fire size summed by month") + #label plot, x axis, y axis
  theme_classic() + #set the theme to classic (removes background and borders etc.)
  theme(plot.title = element_text(face = "bold", hjust = 0.5), plot.caption = element_text(hjust = 0.5)) #set title to center and bold
barGraph

png("Output_BarGraph_GG.png")
barGraph
dev.off()



#####
#Creating maps
bc <- vect(bc_neighbours()) #Get shp of BC bounds
#crs(bc)
#bc <- spTransform(bc, crs("+init=epsg:4326")) #project to WGS84 geographic (Lat/Long)

bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province


#####
#Making Maps with tm package
#Make spatial object out of data
coords <- cbind(df_1995$LONGITUDE, df_1995$LATITUDE) #Store coordinates in new object
#coords <- df_1995[ ,c("LONGITUDE","LATITUDE")]


crs <- ("+init=epsg:4326") #store the coordinate system in a new object, find form original shp? 

firePoints <- vect(coords, atts = df_1995, crs = crs) #Make new spatial Points object using coodinates, data, and projection
#firePoints<- vect(coords)
#firePoints <- vect(cbind(df_1995$LONGITUDE,df_1995$LATITUDE),atts= df_1995, crs= crs)
#firePoints <- SpatialPointsDataFrame(coords = coords, data = df_1995, proj4string = crs)
#firePoints<- vect(df_1995, geom=coords, crs = crs)

map_TM <- tm_shape(sf::st_as_sf(bc)) + #make the main shape
  tm_fill(col = "gray50") +  #fill polygons
  tm_shape(sf::st_as_sf(firePoints)) +
  tm_symbols(col = "red", alpha = 0.25) +
  tm_layout(title = "BC wild-fires 1995", title.position = c("LEFT", "BOTTOM"))

map_TM

png("TmMap.png")
map_TM
dev.off()

#How would you calculate the mean center point location and add it to a map?

lon_sum <- sum(df_1995$LONGITUDE) # make list of lat or sum elements
lat_sum <- sum(df_1995$LATITUDE)# make list of lon ""

num_obs <- nrow(df_1995)
  
mean_center_x <- lon_sum / num_obs
mean_center_y <- lat_sum / num_obs
mean_cntr <- cbind(mean_center_x, mean_center_y)

mean_points <- vect(mean_cntr, atts = NULL, crs = crs)

# conv mean_cntr -> vect data
  

map_W_center<-tm_shape(sf::st_as_sf(bc))+
  tm_fill(col = "gray50") + 
  tm_shape(sf::st_as_sf(firePoints)) +
  tm_symbols(col = "red", alpha = 0.3) +
  tm_shape(sf::st_as_sf(mean_points)) +
  tm_symbols(col = "blue", shape = 24, alpha = 0.6) +
  tm_layout(title = "BC Wild-Fires 1995 ", title.position = c("LEFT", "BOTTOM"))+


#add legend 

tm_add_legend(type = "symbol", 
              labels = c("Fires", "Fires Mean Center"), 
              col = c(adjustcolor( "red", alpha.f = 0.3), 
                      adjustcolor( "blue", alpha = 0.6)), 
              shape = c(19,24))

tm_add_legend
map_W_center
png("map_W_center_leg.png")
map_W_center
dev.off()

#SEE TMAP: GETSTARTED!


