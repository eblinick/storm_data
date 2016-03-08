##sd<- read.csv('repdata-data-StormData.csv', stringsAsFactors = FALSE, gollums)

gollums<- c('EVTYPE','FATALITIES','INJURIES','PROPDMG','PROPDMGEXP','CROPDMG','CROPDMGEXP') 
sd<- read.csv(bzfile('repdata-data-StormData.csv.bz2'),stringsAsFactors = FALSE)
sd<- sd[, gollums]
sd<- subset(sd, FATALITIES!=0 | INJURIES != 0 | PROPDMG != 0 | CROPDMG !=0)
## I am not even trying to categorize this
## for prop about 1/2 of out of 900,000 observations have a propdmgexp
## for crop 284,000 out of 900,000 observations have a cropdmgexp
## I will plot this, but most of the data is missing, guess it is a sample
## adjusting for K, M and B
count(sd$PROPDMGEXP)
count(sd$CROPDMGEXP)
## set default amount to value of prod and crop damage most of the data is in dollars
## 1000 of dollars and millions. I am going to ignore some of the other indicators
## the pdf is not clear about the meaning of '-', '+', 0,2, etc
##sd$PROPAmt <- sd$PROPDMG
##sd$CROPAmt <- sd$CROPDMG
sd$PROPDMGEXP<-toupper(sd$PROPDMGEXP)
sd$CROPDMGEXP<-toupper(sd$CROPDMGEXP)
## convert to actual dollar amount for aggregation
## since there is no consistent data entry for the exponents
## this leads to different results for diferent ways
sd<- mutate(sd, PROPAmt = ifelse(sd$PROPDMGEXP=='H',sd$PROPDMG*100, ifelse(sd$PROPDMGEXP=='K', sd$PROPDMG*10^3, ifelse(sd$PROPDMGEXP=='M', sd$PROPDMG*10^6, ifelse(sd$PROPDMGEXP=='H', sd$PROPDMG*10^9,sd$PROPDMG)))))
sd<- mutate(sd, CROPAmt = ifelse(sd$CROPDMGEXP=='H',sd$CROPDMG*100, ifelse(sd$CROPDMGEXP=='K', sd$CROPDMG*10^3, ifelse(sd$CROPDMGEXP=='M', sd$CROPDMG*10^6, ifelse(sd$CROPDMGEXP=='H', sd$CROPDMG*10^9,sd$CROPDMG)))))            
## since no more than 3 graphs, injuries and fatalities are combined
sd<- mutate(sd, Tot_num = sd$FATALITIES + sd$INJURIES)
## aggregate amounts for injuriest, fatalitites and amounts
hurt<- aggregate(sd$Tot_num~sd$EVTYPE, data=sd, FUN=sum)
cropdmg<- aggregate(sd$CROPAmt~sd$EVTYPE, data=sd, FUN=sum)
propdmg<- aggregate(sd$PROPAmt~sd$EVTYPE, data=sd, FUN=sum)
## rename columns
names(hurt)<- c('event','number')
names(cropdmg)<- c('event','amount') 
names(propdmg)<- c('event','amount')
## take top 10 only
hurt<- hurt[order(-hurt$number),][1:10,]
cropdmg<- cropdmg[order(-cropdmg$amount),][1:10,]
propdmg<- propdmg[order(-propdmg$amount),][1:10,]
## there are a lot more wind relatedevents than any other event, so 
event_type<- count(sd$EVTYPE)
event_type<- event_type[order(-event_type$freq),][1:10,]

colors<- c('red', 'yellow','green','violet', 'blue','orange','pink', 'grey','cyan', 'magenta')
barplot(hurt$number, col=colors, names.arg = hurt$event, main='Fatalities and Injuries by Events', ylab='Number of People',las=3)

## damage plot
par(mfrow=c(1,2))
barplot(propdmg$amount / 10^9, col='blue', names.arg = propdmg$event, main='Property damage by Events',ylab='Damage $',las=3)
barplot(cropdmg$amount /10^9, col='green', names.arg = cropdmg$event, main='Crop Damage by Events',ylab='Damage $',las=3)
par(mfrow=c(1,1))
