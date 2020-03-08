#dplyr, ggplot, magrittr, stringr
library(dplyr)
library(ggplot2)
library(magrittr)
library(stringr)

#Import dataset
Volume_Per_Guest_Property2<-read.csv("VPG_by_TourDateContractDate_(Parameters)_crosstab (2).csv")
Volume_Per_Guest_Property2

#Review beginning of data set
head(Volume_Per_Guest_Property2)

#Define Vector Volume Per Guest 
#set as interger
#remove 0

VPG2<-str_replace_all(Volume_Per_Guest_Property2$VPGnn.Curr, "[^[:alnum:]]", "")
VPG2<-as.integer(VPG2)
VPG2

#Read Class
#Validate as Int
#Summarize
#Print Vector
class(VPG2)
is.integer(VPG2)
summary(VPG2)
round(mean(VPG2), digits = 0)

#Define Property as categorical character vector
Property2<- as.character(Volume_Per_Guest_Property2$TourAccommodationName)
Property2<- na.omit(Property2)
Property2
#Define each property vector 
#Calcuate Occupancy Percentage
GP <- filter(Volume_Per_Guest_Property2, Property2 == "Gold Point")
GP<- as.integer(count(GP)) 
GP

GTL <- filter(Volume_Per_Guest_Property2, Property2 == "Grand Timber")
GTL<- as.integer(count(GTL)) 
GTL

GL7 <- filter(Volume_Per_Guest_Property2, Property2 == "GL7")
GL7 <- as.integer(count(GL7))
GL7

GC8 <- filter(Volume_Per_Guest_Property2, Property2 == "GC8")
GC8<- as.integer(count(GC8))
GC8

BI <- filter(Volume_Per_Guest_Property2, Property2 == "Breck Inn")
BI <-as.integer(count(BI))
BI

SV <- filter (Volume_Per_Guest_Property2, Property2 == "Ski Village")
SV<-as.integer(count(SV))
SV

BR <- filter (Volume_Per_Guest_Property2, Property2 == "Beaver Run")
BR<-as.integer(count(BR))
BR

DT <- filter (Volume_Per_Guest_Property2, Property2 == "DoubleTree")
DT<-as.integer(count(DT))
DT

IMI <- filter (Volume_Per_Guest_Property2, Property2 == "IMI")
IMI<-as.integer(count(IMI))
IMI

BS <- filter (Volume_Per_Guest_Property2, Property2 == "Baymont Suites")
BS<-as.integer(count(BS))
BS

MTT <- filter (Volume_Per_Guest_Property2, Property2 == "Mt. Thunder Lodge")
MTT<-as.integer(count(MTT))
MTT

RI <- filter (Volume_Per_Guest_Property2, Property2 == "Residence Inn")
RI<-as.integer(count(RI))
RI

RQ <- filter (Volume_Per_Guest_Property2, Property2 == "RQ")
RQ<-as.integer(count(RQ))
RQ

#plot(BI, GL7, GC8, GTL, GP, RQ, RI, SV, IMI, MTT, RS, DT)

Total_Units<- (sum(GC8, GL7, GTL,GP,BI, Partners))
Total_Units

Partners<- (sum(SV, BR, DT, BR, MTT, IMI,RI, RQ))
Partners

OccupancyPP<- Partners/Total_Units
OccupancyPP<-round(OccupancyPP, digits = 2)
OccupancyPP

OccupancyGTL<- (GTL/Total_Units)
OccupancyGTL<-round (OccupancyGTL, digits=2)
OccupancyGTL

OccupancyGL7<- (GL7/Total_Units)
OccupancyGL7<- round (OccupancyGL7, digits=2)
OccupancyGL7

OccupancyGC8<- (GC8/Total_Units)
OccupancyGC8<- round (OccupancyGC8, digits=2)
OccupancyGC8

OccupancyBI<- (BI/Total_Units)
OccupancyBI<- round (OccupancyBI, digits=2)
OccupancyBI

#Set Show_Count Vector
#Remove NA's from variable
#Define as boolean
#Print 
#Verify is properly classed, verify is logical
Show_Count2<- (Volume_Per_Guest_Property2$Close.Rate.Curr)
Show_Count2<- (as.logical(Show_Count2))
Show_Count2
class(Show_Count2)
is.logical(Show_Count2)

#Define Avg_Sale Vector
#Remove $ & , sign
#Set vector as numeric
#Round digits of vector
#Print Avg_Sale

Avg_Sale<-(Volume_Per_Guest_Property2$Average.Sale.Curr)
Avg_Sale<-str_replace_all(Avg_Sale, "[^[:alnum:]]", "")
Avg_Sale<-as.numeric(Avg_Sale)
Avg_Sale<-round(Avg_Sale, digits=0)
Avg_Sale
class(Avg_Sale)
is.numeric(Avg_Sale)

#Set Volume vector
#Remove special characters for $ and for ,
#as an interger
#Print Volume
#test Class
#test interger

Volume2<-str_replace_all(Volume_Per_Guest_Property2$Average.Sale.Curr, "[$,]", "")
#Volume2<- na.omit(Volume2)
Volume2<-(as.integer(Volume2))
Volume2

#Set Has Lodging Boolean vector
#as logical
#test class
#verify is logical

HasLodging<-(Volume_Per_Guest_Property2$TourHasLodging)
HasLodging<-as.logical(HasLodging)
class(HasLodging)
is.logical(HasLodging)

as.num = function(Volume2, na.strings = "NA") {
  stopifnot(is.character(x))
  na = Volume2 %in% na.strings
  Volume2[na] = 0
  Volume2 = as.numeric(Volume2)
  Volume2[na] = NA_real_
summary(Volume2)
Volume2
}
as.numeric(Volume2, na.strings="Volume 2")
Volume2

ggplot(Volume_Per_Guest_Property2, aes(x=Property2, y=VPG2)) +
  geom_boxplot()+
   theme(axis.text.x=element_text(angle = 90, vjust = .05))
str(Volume_Per_Guest_Property2)

plot(Property2, Volume2, xlab= "Property" , ylab = "Volume", xlim= c(100, 300), ylim= c( 2000,600000))

#hist(VPG, HasLodging, col.lab="blue", xlab= "Property", ylab = "Volume")


sd(Avg_Sale, na.rm= FALSE)
#boxplot(VPG2)
#geom_boxplot aes (x  VPG2 y  Property2)
#Ho- Mean VPG of 
?boxplot
t.test(VPG2~HasLodging, mu= 0, alt = "two.sided", conf = 0.95, var.eq=F, paired=F)

#t.test(VPG, data = Volume_Per_Guest_Property, mu= 0, alt = "greater", conf = 0.95, var.eq = FALSE, paired = FALSE)
mod <- lm(VPG2 ~ HasLodging, data=Volume_Per_Guest_Property2)
anova(mod)

ggplot(Volume_Per_Guest_Property2, aes(x=Property2, y=VPG2)) +
  geom_boxplot()+
  theme(axis.text.x=element_text(angle = 90, vjust = .05))

total_sales<-sum(Avg_Sale)
TS<- total_sales/Total_Units

