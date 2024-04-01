print("Final Assignment Milestone 2")
print("Course Name -  ALY6010: Probability Theory and Introductory Statistics")

# Importing rideshare dataset containing 693071 observations with 58 variables
getwd()
ride<-read.csv("rideshare.csv")

# Installing and loading the libraries
library(visdat)
install.packages("tidyr")
library("tidyr")
library(car)
library(dplyr)
library(psych)
install.packages("skimr")
library(skimr)
library(naniar)
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(dplyr)
install.packages("scatterplot3d")
library("scatterplot3d")

# Dropping columns
drop<-c("id","timestamp","product_id","apparentTemperature","long_summary","windGustTime",
        "temperatureHighTime","temperatureLowTime","apparentTemperatureHigh","apparentTemperatureHighTime",
        "apparentTemperatureLow","apparentTemperatureLowTime","icon","dewPoint","pressure","windBearing","cloudCover",	
        "uvIndex","visibility.1","ozone","sunriseTime","sunsetTime","moonPhase","precipIntensityMax",	
        "uvIndexTime","temperatureMin","temperatureMinTime","temperatureMax","temperatureMaxTime",	
        "apparentTemperatureMin","apparentTemperatureMinTime","apparentTemperatureMax","apparentTemperatureMaxTime","timezone")
ride<-ride[,!(names(ride) %in% drop)]

# Visualization and Checking NA values
gg_miss_which(ride)
gg_miss_var(ride)
miss_var_summary(ride)
sum(is.na(ride))
sum(is.null(ride))
complete.cases(ride)
which(!complete.cases(ride)) 
na<-which(!complete.cases(ride))  
ride1<- ride[-na,]

# Changing the column names
colnames(ride1)[6]<-"pickup_location"
colnames(ride1)[7]<-"drop_location"
colnames(ride1)[9]<-"cab_category"
colnames(ride1)[16]<-"conditions"

# Checking for duplicated rows and removing them
duplicated(ride1)
anyDuplicated(ride1)
ride1<-ride1[!duplicated(ride1), ]

# Seperating date and time into different columns
ride1<-ride1 %>%
  separate(datetime, c("date", "time"), " ")

# Adding columns 
ride1$price_per_distance<-round(ride1$price/ride1$distance,2)
ride1<-ride1 %>% relocate(price_per_distance,.after = distance)
ride1$price_without_surge<-round(ride1$price/ride1$surge_multiplier,1)
ride1<-ride1 %>% relocate(price_without_surge,.after = price)

# checking outliers
boxplot(ride1$price)
ride1<-subset(ride1,price!=97.5)

# Replacing hours to Morning, Afternoon,Evening and Night
#0-5 night
#6-11 morning
#12-17 afternoon
#18-24 evening
ride1<-ride1 %>% mutate(hour = recode(hour, '0' = 'Night', '1' = 'Night', '2' = 'Night','3' = 'Night','4' = 'Night','5' = 'Night','6' = 'Morning','7' = 'Morning','8' = 'Morning','9' = 'Morning','10' = 'Morning','11' = 'Morning','12' = 'Afternoon','13' = 'Afternoon','14' = 'Afternoon','15' = 'Afternoon','16' = 'Afternoon','17' = 'Afternoon','18' = 'Evening','19' = 'Evening','20' = 'Evening','21' = 'Evening','22' = 'Evening','23' = 'Evening'))

# Dropping day column (numeric)
drop2<-c("day")
ride1<-ride1[,!(names(ride1) %in% drop2)]

# Changing the datatypes
ride1$hour<-as.factor(ride1$hour)
ride1$pickup_location<-as.factor(ride1$pickup_location)
ride1$drop_location<-as.factor(ride1$drop_location)
ride1$conditions<-as.factor(ride1$conditions)
ride1$cab_type<-as.factor(ride1$cab_type)
ride1$cab_category<-as.factor(ride1$cab_category)

# Analysis
headTail(ride1)
str(ride1)
summary(ride1)
dim(ride1)
skim(ride1)
describe(ride1,quant = c(0.25, 0.75),IQR = T)
glimpse(ride1)
describeBy(ride1,group=ride1$cab_type,quant = c(0.25, 0.75), IQR = T)

# Normal QQplots
qqnorm(ride1$price, pch = 1, frame = FALSE,main="Q-Q Plot (Price)")
qqline(ride1$price, col = "tomato", lwd = 2)
qqnorm(ride1$distance, pch = 1, frame = FALSE,main="Q-Q Plot (Distance)")
qqline(ride1$distance, col = "blue", lwd = 2)

#Density plots
normality_price <- ggdensity(ride1$price, main = "Density plot of Price", xlab = "Price", fill = "#baf54c")
normality_distance <- ggdensity(ride1$distance, main = "Density plot of Distance", xlab = "Distance", fill = "#4cf5bd")
normality_surge.multiplier <- ggdensity(ride1$surge_multiplier,xlim=c(1,1.25), main = "Density plot of surge Multiplier", xlab = "Surge Multiplier", fill = "#40c7f7")
normality_price.per.distance <- ggdensity(ride1$price_per_distance,xlim=c(0,100), main = "Density plot of Price Per Distance", xlab = "Price Per Distance", fill = "#d27afa")
grid.arrange(normality_price, normality_distance , normality_surge.multiplier, normality_price.per.distance)

# Boxplots
ggplot(ride1, aes(x=cab_type,y=price, fill=cab_type)) + geom_boxplot() +scale_fill_brewer(palette = "Spectral") + theme(text = element_text(size = 20))+ ylab("Price")+ggtitle("Boxplot")
ggplot(ride1, aes(x=cab_type,y=distance, fill=cab_type)) + geom_boxplot() + scale_fill_brewer(palette = "Dark2") + theme(text = element_text(size = 20))+ylab("Distance") + ggtitle("Boxplot")

#Hypothesis testing

# Subgroups
Uber<-subset(ride1,cab_type=="Uber")
Lyft<-subset(ride1,cab_type=="Lyft")


#Two sample T-test
#Test 1
#Both Uber and Lyft have the same mean surge multiplier 
t.test(Uber$surge_multiplier ,Lyft$surge_multiplier,var.equal = FALSE)

#Test 2
#Both Uber and Lyft have the same mean price 
t.test(Uber$price ,Lyft$price,var.equal = FALSE)

#Test 3
#Both Uber and Lyft have the same mean distance covered
t.test(Uber$distance ,Lyft$distance,var.equal = FALSE)

#Test 4
#Both Uber and Lyft have the same mean price per distance 
t.test(Uber$price_per_distance ,Lyft$price_per_distance,var.equal = FALSE)

#Test 5
#Both Uber and Lyft have the same mean price without surge.
t.test(Uber$price_without_surge ,Lyft$price_without_surge,var.equal = FALSE)

# One sample T-test

#Test 1
#Null Hypothesis: mean price is greater than 20.
#Alternate hypothesis: mean price is < than or = 20
t.test(ride1$price, mu=20, alternative = "less")

#Test 2
#Null Hypothesis: mean distance covered is lesser than 1.5
#Alternate hypothesis: mean distance covered is > than or = 1.5
t.test(ride1$distance, mu=1.5, alternative = "greater")


#Test 3
#Null Hypothesis: mean surge multiplier is lesser than 2
#Alternate hypothesis: mean distance covered is > than or = 2
t.test(ride1$surge_multiplier, mu=2, alternative = "greater")

#Test 4
#Null Hypothesis: mean price per distance is greater than 5
#Alternate hypothesis: mean price per distance is < than or = 5
t.test(ride1$price_per_distance, mu=5,alternative = "less")

#Test 5
#Null Hypothesis: mean temperature is  = 45
#Alternate hypothesis: mean temperature is  != 45
t.test(ride1$temperature, mu=45)

# Data Visualizations
# Graph 1
df1<-ride1 %>% group_by(conditions,cab_type) %>% summarize(count=n())
ggplot(df1, aes(conditions,count, fill = cab_type)) + 
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer(palette="Accent")+ggtitle("Grouped Bar chart")+xlab("Conditions")+ylab("Count")+labs(fill = "Cab Type")+theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Graph 2
require(ochRe)
library(ochRe)
df2<-ride1 %>% group_by(pickup_location,cab_type) %>% summarize(count=n())
ggplot(df2, aes(count,pickup_location, fill = cab_type)) + 
  geom_bar(stat = "identity",colour = "black")+
  scale_fill_ochre(palette="mccrea")+xlim(0,60000)+ggtitle("Stacked Bar chart")+xlab("Count")+ylab("Location")+labs(fill = "Cab Type")+theme(axis.text.x = element_text( hjust = 1))

# Graph 3
library(gridExtra)
Uber<-subset(ride1,cab_type=="Uber")
Lyft<-subset(ride1,cab_type=="Lyft")
Den1<-ggplot(Uber, aes(x=price, group=cab_category, fill=cab_category)) +
  geom_density(adjust=4, alpha=.4) +xlim(0,60)+xlab("Price")+ylab("Density")+labs(fill = "Cab Category")+theme_ipsum()
Den2<-ggplot(Lyft, aes(x=price, group=cab_category, fill=cab_category)) +
  geom_density(adjust=4, alpha=.4) +xlim(0,60)+xlab("Price")+ylab("Density")+labs(fill = "Cab Category")+theme_ipsum()
grid.arrange(Den1,Den2,top=textGrob("Multiple density charts of Price for various Cab Categories"))

# Graph 4  
cols <- c("dark blue",  "orange")
with(ride1, scatterplot3d(price,distance,surge_multiplier,
                          main="3 D Plot",xlab = "Price",ylab = "Distance",zlab = "Surge Multiplier",pch = 16, color=cols[as.numeric(ride1$cab_type)]))
legend("top",inset = c(0, 1), legend = levels(ride1$cab_type),col =cols, pch =16,xpd=TRUE,horiz=TRUE,bty = "n")

# Graph 5
library(wesanderson)
ggplot(ride1, aes(hour,price, col=cab_type))+ scale_color_manual(values= wes_palette("GrandBudapest1", n = 2))+
  geom_point(size=0.5,position = position_jitter(width = .15, height = 4))+ggtitle("Jitter plot")+xlab("Hour")+ylab ("Price")+labs(color = "Cab Type")

# Graph 6
ride1$Day <- factor(ride1$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(ride1, aes(x=`Day`, y=`price`, fill=cab_type)) +
  geom_boxplot() + facet_wrap(~cab_type)+
  theme_ipsum() +scale_fill_ochre("qalah")+labs(fill = "Cab Type")+ggtitle("Boxplots")+xlab("Days")+ylab("Price")+
  theme(axis.text.x = element_text(angle=90, hjust=1))

# Graph 7
ggplot(ride1, aes(`conditions`, `cab_category`, fill= `price`)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_distiller(palette = "Spectral") + ggtitle("Heat Map")+xlab("Conditions")+ylab("Cab Category")+labs(fill = "Price")

# Graph 8
df5<-ride1 %>% group_by(Day,cab_type) %>% summarize(count=n())
ggplot(df5, aes(Day,count,fill=cab_type)) +
  geom_col(position = "dodge")+coord_polar()+
  scale_fill_ochre(palette="parliament")+ggtitle("Circular Bar chart")+xlab("Days of the week")+ylab("Count")+labs(fill = "Cab Type")+theme(axis.text.x = element_text( hjust = 1))

# Graph 9
install.packages("highcharter")
library(highcharter)
library(dplyr)
df4<-ride1 %>% group_by(cab_type,hour,cab_category) %>% summarize(count=n())
dout<-data_to_hierarchical(df4,c(hour,cab_type,cab_category),count)
hchart(dout,type="sunburst")

# Graph 10
df3<-ride1 %>% group_by(surge_multiplier,distance,price,cab_type) %>% summarize(count=n())
library(viridis)
colours = c( "#F5A2A2",  "#4682B4")
ggplot( df3,aes(x=distance, y=price,size=surge_multiplier, color=cab_type)) +
  geom_point(alpha=0.7) +scale_color_manual(values = colours) +scale_size(range = c(.1, 2))+
  ggtitle("Price vs Distance for both cab types") +
  theme_ipsum() +ylab("Price")+xlab("Distance")

# Graph 11
df6 <- ride1 %>%
  filter(surge_multiplier > 1.00) %>%
  group_by(hour,surge_multiplier) %>%
  dplyr::summarize(total_rides=n())

p<-ggplot(data = df6, aes(x = hour, y = total_rides, fill = factor(surge_multiplier))) +
  geom_bar(stat = "identity", color = "black",lwd = 1, show.legend = TRUE)
p + coord_polar(theta = "y")

# Regression chart
model<-subset(ride1,surge_multiplier<=1)
Var1 <- model %>%group_by(price, distance)

Var2 <- Var1 %>%group_by(cab_type, distance) %>%summarize_at(vars(price), list(cab_category = mean))


col3<-c("brown1","royalblue4")
Reg1<-ggplot(Var2, aes(distance, cab_category, color=cab_type)) +
  xlab("Distance") + ylab("Price")+geom_point(size=.5,shape=4)+scale_color_manual(values = col3)+geom_smooth(method = "lm",se=FALSE)
Reg2<-ggplot(Var2, aes(distance, cab_category)) +xlab("Distance") + ylab("Price")+geom_point(size=.5,shape=4)+scale_color_manual(values = col3)+geom_smooth(method = "lm",se=FALSE)
grid.arrange(Reg1,Reg2)

# Histograms

library(ggpubr)
hist1<-ggplot( ride1,aes(x=price)) +geom_histogram(binwidth = 5,fill= "#00A9FF",colour = "black")
hist2<-ggplot( ride1,aes(x=distance)) +geom_histogram(binwidth = 0.5,fill= "#A54657",colour = "black")
hist3<-ggplot( ride1,aes(x=surge_multiplier)) +geom_histogram(binwidth = 0.15,fill= "#97ce4c",colour = "black")
hist4<-ggplot( ride1,aes(x=temperature)) +geom_histogram(binwidth = 2.5,fill= "#00C19A",colour = "black")
hist5<-ggplot( ride1,aes(x=humidity)) +geom_histogram(binwidth = 0.04,fill= "#F39B7FFF",colour = "black")
hist6<-ggplot( ride1,aes(x=precipIntensity)) +geom_histogram(binwidth = 0.01,fill= "#938dd2",colour = "black")
hist7<-ggplot( ride1,aes(x=windSpeed)) +geom_histogram(binwidth = 1,fill= "#F8766D",colour = "black")
hist8<-ggplot( ride1,aes(x=windGust)) +geom_histogram(binwidth = 1.5,fill= "#00BFC4",colour = "black")
hist9<-ggplot( ride1,aes(x=visibility)) +geom_histogram(binwidth = 0.75,fill= "#FF68A1",colour = "black")
grid.arrange(hist1,hist2,hist3,hist4,hist5,hist6,hist7,hist8,hist9,top=textGrob("Histograms of Variables"))

# Wordcloud 1
ride2=as.character(ride1$drop_location)
word.corpus<-Corpus(VectorSource(ride2))
word.corpus<-word.corpus%>% tm_map(removePunctuation)%>% 
  tm_map(removeNumbers)%>%
  tm_map(stripWhitespace)%>%
  tm_map(tolower)%>%
  tm_map(stopwords("english"))
countofwords<-as.matrix(TermDocumentMatrix(word.corpus))
freqofwords<-sort(rowSums(countofwords), decreasing=TRUE)
head(freqofwords)
library(wesanderson)
dev.new(width=10, height=5, unit="in")
wordcloud(words=names(freqofwords), freq=freqofwords,scale=c(4, .5),max.words = 100, random.order = FALSE,color=wes_palette("GrandBudapest1"))

# Wordcloud 2
ride3=as.character(ride1$cab_category)
word.corpus2<-Corpus(VectorSource(ride3))
word.corpus2<-word.corpus2%>% tm_map(removePunctuation)%>% 
  tm_map(removeNumbers)%>%
  tm_map(stripWhitespace)%>%
  tm_map(tolower)%>%
  tm_map(stopwords("english"))
countofwords2<-as.matrix(TermDocumentMatrix(word.corpus2))
freqofwords2<-sort(rowSums(countofwords2), decreasing=TRUE)
head(freqofwords2)
dev.new(width=10, height=5, unit="in")
wordcloud(words=names(freqofwords2), freq=freqofwords2,scale=c(4, .5),max.words = 100, random.order = FALSE,color=wes_palette("Zissou1"))

# Maps
lat1<-c(42.364,42.3661,42.352,42.3661,42.3398,42.3647,42.3503,42.3588,42.3505,42.3559,42.3505,42.3519)
lon1<-c(-71.060,-71.0892,-71.065,-71.1054,-71.0810,-71.0551,-71.1054,-71.0707,-71.0542,-71.0550,-71.0631,-71.0631)
area<-c("North Station","North End","West End","Financial District","Beacon Hill","Fenway", "South Station",
        "Theatre District","Back Bay", "Boston University","Northeastern University","Haymarket Square")
map1<-data.frame(lat1,lon1,area)
bostonmap<-get_stamenmap(bbox = c(left=-71.26,right=-70.87,top= 42.45,bottom=42.25),
                         maptype = "terrain",zoom=12)
ggmap(bostonmap) +geom_point(data=map1, aes(x=lon1, y=lat1),size=0.5)+
  labs(x = 'Longitude', y = 'Latitude') +
  geom_label_repel(data=map1, aes(x = lon1, y= lat1,
                                  label = area), fill = "grey",
                   label.size = 0, family="mono",
                   min.segment.length = 0,
                   max.overlaps = Inf,
                   box.padding = unit(.6, "lines"),
                   label.padding = unit(.10, "lines"),
                   segment.color = "black", segment.size = .5)
theme_map()

# Ubermap
ggmap(bostonmap) + stat_density_2d(data=Uber, aes(x=longitude, y=latitude,fill=..level..), geom="polygon",bins=150,alpha=.1) +
  ggtitle("Density Map of Uber rides in Boston")+scale_fill_viridis_c(option = "inferno")
grid.arrange(Boston,Uber)

# LyftMap
ggmap(bostonmap) + stat_density_2d(data=Lyft, aes(x=longitude, y=latitude,fill=..level..),bins=150,alpha=.1, geom="polygon") +
  ggtitle("Density Map of Lyft Rides in Boston")+ scale_fill_viridis_c(option = "turbo")

# Correlation Matrix
corr<-ride1 %>% select(temperature,temperatureHigh,temperatureLow,price)
corrplot(cor(corr),addCoef.col = "white",number.cex = 0.8,
         number.digits = 2,bg="white",outline = "black",addgrid.col = "black")

corr1<-ride1 %>% select(precipIntensity,precipProbability,humidity,windSpeed,windGust,visibility,price)
corrplot(cor(corr1),addCoef.col = "white",number.cex = 0.8,
         number.digits = 2,bg="grey",outline = "black",addgrid.col = "white",method="shade")
corr2<-ride1 %>% select(latitude,longitude,price)
corrplot(cor(corr2),addCoef.col = "white",number.cex = 0.8,
         number.digits = 2,bg="grey",outline = "black",addgrid.col = "white",method = 'color')
corr3<-ride1 %>% select(distance,surge_multiplier,price)
corrplot(cor(corr3),addCoef.col = "red",number.cex = 0.8,
         number.digits = 2,bg="white",outline = "black",addgrid.col = "grey",method = 'pie')

# Regression Models
model1<-lm(formula = price ~ distance+Day, data = ride1)
summary(model1)
model2<-lm(formula = price ~ distance+surge_multiplier, data = ride1)
summary(model2)
model3<-lm(formula = price ~ distance+surge_multiplier+price_per_distance,data = ride1)
summary(model3)
model4<-lm(formula = price ~ distance+surge_multiplier+cab_category, data = ride1)
summary(model4)
model5<-lm(formula = price ~ distance+surge_multiplier+price_without_surge+cab_category, data = ride1)
summary(model5)
