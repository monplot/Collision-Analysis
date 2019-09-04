

#Loading necessary libraries 
rm(list = ls())
library(data.table)
library(corrplot)
library(gapminder)
library(magrittr)
library(ggplot2)
library(dplyr)
library(forcats)

#reading data
nyc=read.csv('C:\\Users\\monis\\Downloads\\NYPD_Motor_Vehicle_Collisions (1).csv')
nyc <- na.omit(nyc)
nyc$PERSONS=nyc$NUMBER.OF.PERSONS.INJURED+nyc$NUMBER.OF.PERSONS.KILLED

# manipulating data for vizualisation 

cont1 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.1)
cont2 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.2)
cont3 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.3)
cont4 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.4)
cont5 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.5)

per1=data.frame(nyc$PERSONS)
per2=data.frame(nyc$PERSONS)
per3=data.frame(nyc$PERSONS)
per4=data.frame(nyc$PERSONS)
per5=data.frame(nyc$PERSONS)

head(nyc$PERSONS,10)

persons_by_fac <- data.frame(rbind(per1,per2,per3,per4,per5))
contributing_fac <- data.frame(rbind(cont1,cont2,cont3,cont4,cont5))


contributing_fac_count <- cbind(persons_by_fac,contributing_fac)
library(dplyr)
contributing_fac_count_plotdata <- filter(contributing_fac_count, fac!='Unspecified')
head(contributing_fac_count)
colnames(contributing_fac_count)

top10 <- contributing_fac_count_plotdata %>%
            group_by(fac) %>%
            summarize(sum=sum(nyc.PERSONS)) %>%
            arrange(desc(sum))%>%
            head(n=10)

top10 <-top10[-1,]
plot <- ggplot(top10, aes(y=sum, x=factor(fac))) +geom_bar(stat="identity")
#####################################################################################################################


cont1 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.1)
cont2 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.2)
cont3 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.3)
cont4 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.4)
cont5 <- data.frame(fac=nyc$CONTRIBUTING.FACTOR.VEHICLE.5)

per1=data.frame(nyc$PERSONS)
per2=data.frame(nyc$PERSONS)
per3=data.frame(nyc$PERSONS)
per4=data.frame(nyc$PERSONS)
per5=data.frame(nyc$PERSONS)

bor1=data.frame(nyc$BOROUGH)
bor2=data.frame(nyc$BOROUGH)
bor3=data.frame(nyc$BOROUGH)
bor4=data.frame(nyc$BOROUGH)
bor5=data.frame(nyc$BOROUGH)

head(nyc$PERSONS,10)

persons_by_fac <- data.frame(rbind(per1,per2,per3,per4,per5))
contributing_fac <- data.frame(rbind(cont1,cont2,cont3,cont4,cont5))
bor_fac <- data.frame(rbind(bor1,bor2,bor3,bor4,bor5))

contributing_fac_count <- cbind(persons_by_fac,contributing_fac,bor_fac)

contributing_fac_count_plotdata <- filter(contributing_fac_count, fac!='Unspecified')
head(contributing_fac_count)
colnames(contributing_fac_count)

top10 <- contributing_fac_count_plotdata

top10 <- contributing_fac_count_plotdata %>%
  group_by(fac,nyc.BOROUGH) %>%
  summarize(sum=sum(nyc.PERSONS)) %>%
  arrange(-sum)


# vizualising top 10 factors as bar chart

top10 <-top10[-(1:5),]
#top10 <- top10[order(top10$fac),]
top10 <- filter(top10,sum(sum)>5000)
colnames(top10)
top10=filter(top10,fac!="Fatigued/Drowsy")
top10=filter(top10,fac!="Physical Disability")
range(top10$sum)
(plot <- ggplot(top10, aes(y=sum, x=fct_rev(fct_infreq(factor(fac))))) +geom_bar(aes(fill = nyc.BOROUGH),stat="identity")+
    coord_flip())+  ggtitle("FACTORS BY BOROUGH") + scale_fill_manual(values = c("#FFFFCB","#C9FFD5","#74CEB7","#2C858D","#004056"))+
  xlab("FACTORS") + ylab("COLLISIONS")+theme_minimal()+
  theme(
    plot.title = element_text(color="BLACK", size=14, face="bold.italic"),
    axis.title.x = element_text(color="BLACK", size=14, face="italic"),
    axis.title.y = element_text(color="BLACK", size=14, face="italic")
  )+labs(fill="BOROUGH")

###########################################################################

# vizualising heat map for collions throught the day

nyc$day <- weekdays(as.Date(nyc$DATE,'%m/%d/%Y'))
table(nyc$day)
nyc$hour=gsub("\\:.*","",nyc$TIME)
unique(nyc$hour)
nyc$hour=as.numeric(nyc$hour)
nyc$sumofcollisions=nyc$`NUMBER OF PERSONS INJURED`+nyc$`NUMBER OF PERSONS KILLED`
nyc$hour=as.factor(nyc$hour)
nyc$day=as.factor(nyc$day)
heatmapdata <- nyc%>%
  group_by(nyc$hour,nyc$day)%>%
  summarize(count=n())
colnames(heatmapdata) = c("hour", "day", "tot")
ggplot(heatmapdata, aes(x=factor(day), y=factor(hour)) + 
  geom_tile(aes(fill = tot),colour = "white") + 
  scale_fill_gradient(low = "white",high = "steelblue")                                                                                                    +     high = "steelblue")

(p <- ggplot(heatmapdata, aes(factor(day), factor(hour))) + geom_tile(aes(fill = tot),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue"))+ geom_text(aes(label = round(tot, 1)))



#########################################################################################################################################################################################

# parallel plot for comparing borough wise collisions
library(dplyr)
library(magrittr)
unique(nyc$BOROUGH)
nyc$pedestrians <- nyc$NUMBER.OF.PEDESTRIANS.INJURED+nyc$NUMBER.OF.PEDESTRIANS.KILLED
nyc$cycle <- nyc$NUMBER.OF.CYCLIST.INJURED+nyc$NUMBER.OF.CYCLIST.KILLED
nyc$motor <- nyc$NUMBER.OF.MOTORIST.INJURED+nyc$NUMBER.OF.MOTORIST.KILLED
parplot=nyc%>%
  group_by(`BOROUGH`)%>%
  summarise(ped = sum(pedestrians,na.rm = TRUE),motor = sum(motor,na.rm = TRUE),cycle=sum(cycle,na.rm = TRUE))
parplot=nyc%>%
  group_by(`BOROUGH`)%>%
  summarise(ped = sum(`NUMBER OF PEDESTRIANS KILLED`,na.rm = TRUE),motor = sum(`NUMBER OF MOTORIST KILLED`,na.rm = TRUE),cycle=sum(`NUMBER OF CYCLIST KILLED`,na.rm = TRUE))

parplot=as.data.frame(parplot)
library(GGally)
ggparcoord(parplot, columns = 2:4, groupColumn = 'BOROUGH', scale = 'globalminmax',title = "PARALLEL COMPARISON OF BOROUGHS")+theme_minimal()+
  xlab("TYPE OF COLLISON") + ylab("COLLISIONS")+theme_light()+
  theme(
    plot.title = element_text(color="BLACK", size=14, face="bold.italic"),
    axis.title.x = element_text(color="BLACK", size=14, face="italic"),
    axis.title.y = element_text(color="BLACK", size=14, face="italic")
  )+labs(fill="BOROUGH")

##################

# borough wise heatmap 
nyc$hour=gsub("\\:.*","",nyc$TIME)
unique(nyc$hour)

nyc$hour=as.numeric(nyc$hour)
nyc$sumofcollisions=nyc$NUMBER.OF.PERSONS.INJURED+nyc$NUMBER.OF.PERSONS.KILLED
nyc$hour=as.factor(nyc$hour)
nyc$BOROUGH=as.factor(nyc$BOROUGH)
heatmapdata <- nyc%>%
  group_by(nyc$hour,nyc$BOROUGH)%>%
  summarize(count=n())

colnames(heatmapdata) = c("hour", "BOROUGH", "tot")
heatmapdata=as.data.frame(heatmapdata)
heatmapdata$hour=as.factor(heatmapdata$hour)
(p <- ggplot(heatmapdata, aes(factor(BOROUGH), factor(hour))) + geom_tile(aes(fill = tot),colour = "white") + scale_fill_gradient(low = "white",high = "#74CEB7")+ggtitle("HOURLY ACCIDENTS BY BOROUGH") +
    xlab("BOROUGH") + ylab("HOUR")+theme_minimal()+
    theme(
      plot.title = element_text(color="BLACK", size=14, face="bold.italic"),
      axis.title.x = element_text(color="BLACK", size=14, face="italic"),
      axis.title.y = element_text(color="BLACK", size=14, face="italic")
    )+labs(fill="COLLISION"))


ggplot(heatmapdata, aes(factor(heatmapdata$BOROUGH), factor(heatmapdata$hour))) + geom_tile(aes(fill = tot),colour = "white") + scale_fill_gradient(low = "white",high = "red")+ geom_text(aes(label = round(tot, 1)))


theme(
  plot.title = element_text(color="BLACK", size=14, face="bold.italic"),
  axis.title.x = element_text(color="BLACK", size=14, face="bold.italic"),
  axis.title.y = element_text(color="BLACK", size=14, face="bold.italic")
)
########################################################


# Bubble plot comparing non-fatal and fatal accidents by region/borough 

library(dplyr)
nyc <- na.omit(nyc)
fatal_accidents=filter(nyc,nyc$NUMBER.OF.PERSONS.KILLED!=0)
all_accidents_count=data.frame(count(nyc,BOROUGH))
fatal_accidents_count=data.frame(count(fatal_accidents,BOROUGH))
install.packages("devtools")
devtools::install_github("jcheng5/bubbles")
library(bubbles)
bubbles(value = all_accidents_count$n, label = all_accidents_count$BOROUGH,
        color =c("99FFFF","006666","669999","669999","CCFFFF"))
color =c("CCFFFF","99FFFF","33FFFF","00CCCC","006666")
fatal_accidents_count$BOROUGH
bubbles(value = fatal_accidents_count$n, label = fatal_accidents_count$BOROUGH,
        color =c("99FFFF","006666","33CCCC","669999","CCFFFF"))





