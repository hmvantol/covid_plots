library('dplyr')
library('ggplot2')
library('gganimate')

setwd('/Users/hmvantol/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/')

files<- list.files()
files<- files[1:length(files)-1]

d1 <- NULL
for (f in 1:length(files)) {
	file<- read.csv(files[f])
	# print(colnames(file))
	if ('Last_Update' %in% colnames(file)) {
		colnames(file)[which(colnames(file) == 'Province_State')] <- 'Province.State'
		colnames(file)[which(colnames(file) == 'Country_Region')] <- 'Country.Region'
		colnames(file)[which(colnames(file) == 'Last_Update')] <- 'Last.Update'
	}
	# print(files[f])
	d1<- rbind(d1,file[,c("Province.State","Country.Region","Last.Update","Confirmed","Deaths","Recovered")])
}

US<- d1[d1$Country.Region %in% c('US','Italy') | d1$Province.State == 'Hubei',]
US$Province.State <- as.character(US$Province.State)
US$Country.Region <- as.character(US$Country.Region)
US$Province.State[US$Country.Region == 'Italy'] <- 'Italy'

US$Std.Time <- strptime(US$Last.Update,format="%Y-%m-%d %H:%M:%S",tz='UTC')
US$Std.Time[which(is.na(US$Std.Time) == T)] <- strptime(US$Last.Update[which(is.na(US$Std.Time) == T)],format="%Y-%m-%dT%H:%M:%S",tz='UTC')
US$Std.Time[which(is.na(US$Std.Time) == T)] <- strptime(US$Last.Update[which(is.na(US$Std.Time) == T)],format="%m/%d/%y %H:%M",tz='UTC')
US$Std.Time[which(is.na(US$Std.Time) == T)] <- strptime(US$Last.Update[which(is.na(US$Std.Time) == T)],format="%m/%d/%Y %H:%M",tz='UTC')

US<- US[order(US$Std.Time),]
US$Std.Time<- as.POSIXct(US$Std.Time)

US$Province.State[which(US$Province.State == 'Virgin Islands, U.S.')] <- 'Virgin Islands'
states<- t(as.data.frame(strsplit(as.character(US$Province.State),', ')))[,2]
row.names(states)<- NULL
US$states<- states

US$states[which(US$states == 'IL')] <- 'Illinois'
US$states[which(US$states == 'MA')] <- 'Massachusetts'
US$states[which(US$states == 'WA')] <- 'Washington'
US$states[which(US$states == 'AZ')] <- 'Arizona'
US$states[which(US$states == 'CA')] <- 'California'
US$states[which(US$states == 'WI')] <- 'Wisconsin'
US$states[which(US$states == 'TX')] <- 'Texas'
US$states[which(US$states == 'NE')] <- 'Nebraska'
US$states[which(US$states == 'OR')] <- 'Oregon'
US$states[which(US$states == 'OR ')] <- 'Oregon'
US$states[which(US$states == 'NY')] <- 'New York'
US$states[which(US$states == 'FL')] <- 'Florida'
US$states[which(US$states == 'NH')] <- 'New Hampshire'
US$states[which(US$states == 'NC')] <- 'North Carolina'
US$states[which(US$states == 'NJ')] <- 'New Jersey'
US$states[which(US$states == 'UT')] <- 'Utah'
US$states[which(US$states == 'CO')] <- 'Colorado'
US$states[which(US$states == 'VA')] <- 'Virginia'
US$states[which(US$states == 'RI')] <- 'Rhode Island'
US$states[which(US$states == 'GA')] <- 'Georgia'
US$states[which(US$states == 'NV')] <- 'Nevada'
US$states[which(US$states == 'MN')] <- 'Minnesota'
US$states[which(US$states == 'KS')] <- 'Kansas'
US$states[which(US$states == 'TN')] <- 'Tennessee'
US$states[which(US$states == 'MD')] <- 'Maryland'
US$states[which(US$states == 'IN')] <- 'Indiana'
US$states[which(US$states == 'SC')] <- 'South Carolina'
US$states[which(US$states == 'MO')] <- 'Missouri'
US$states[which(US$states == 'OK')] <- 'Oklahoma'
US$states[which(US$states == 'CT')] <- 'Connecticut'
US$states[which(US$states == 'VT')] <- 'Vermont'
US$states[which(US$states == 'D.C.')] <- 'District of Columbia'
US$states[which(US$states == 'IA')] <- 'Iowa'
US$states[which(US$states == 'PA')] <- 'Pennsylvania'
US$states[which(US$states == 'KY')] <- 'Kentucky'
US$states[which(US$states == 'HI')] <- 'Hawaii'
US$states[which(US$states == 'LA')] <- 'Louisiana'
US$states[which(US$states == 'Chicago')] <- 'Illinois'
US$states[which(US$states == 'United States Virgin Islands')] <- 'Virgin Islands'


state_names<- c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware','Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming','District of Columbia','Marshall Islands','Puerto Rico','Virgin Islands','Guam','American Samoa','Northern Mariana Islands','Hubei','Italy')

US$states[!US$states %in% state_names]<- 'Other'
US<- US[-which(US$states == 'Other'),]
US$Confirmed[is.na(US$Confirmed) == T] <- 0
US$Deaths[is.na(US$Deaths) == T] <- 0
US$Recovered[is.na(US$Recovered) == T] <- 0
# US$Total = US$Confirmed + US$Deaths + US$Recovered
US$Total = US$Confirmed

US <- US[,c('Std.Time','states','Total')]
US<- US[!duplicated(US),]

require(tidyverse)
d2<- US %>% 
	group_by(State=states,Day=format(Std.Time,'%Y-%m-%d')) %>%
	# group_by(State=states,Day=format(Std.Time,'%Y-%U')) %>%
	summarize_if(is.numeric,max) %>%
	mutate(Change=Total-dplyr::lag(Total,n=7))


# tail(d2[d2$State=='Hubei',])
d3<- d2[which(d2$State %in% c('Washington','Texas','California','New York','Oregon','Italy','Hubei')),]
d3$State <- factor(d3$State,levels=c('Washington','Texas','California','New York','Oregon','Italy','Hubei'))

# d3<-d2
# d3$State <- factor(d3$State,levels=state_names)

ggplot(d3, aes(x=Total, y=Change, colour=State, label=State)) + geom_line() +
 scale_x_log10() + scale_y_log10() +
 scale_colour_manual(values=c(rep('grey',length(unique(d3$State))-2),'red','red')) +
 layer(data=d3[which(d3$Day == d3$Day[length(d3$Day)]),], geom='point',stat='identity',position='identity', params=list(colour='red')) +
 layer(data=d3[which(d3$Day == d3$Day[length(d3$Day)]),], geom='text',stat='identity',position='identity', params=list(colour='black',vjust=0, hjust=0)) +
 coord_fixed(xlim=c(min(d3$Total,na.rm=T),max(d3$Total,na.rm=T)*1.5), ylim=c(min(d3$Total,na.rm=T),max(d3$Total,na.rm=T)*1.5),ratio=1) +
 labs(x='Total confirmed cases', y='New confirmed cases (in the last week)') + 
 theme_bw() + theme(legend.position='none')
