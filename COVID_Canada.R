library('dplyr')
library('ggplot2')
library('gganimate')
library('gifski')

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

Canada<- d1[d1$Country.Region %in% c('Canada','Italy') | d1$Province.State == 'Hubei',]
Canada$Province.State <- as.character(Canada$Province.State)
Canada$Country.Region <- as.character(Canada$Country.Region)
Canada$Province.State[Canada$Country.Region == 'Italy'] <- 'Italy'


Canada$Std.Time <- strptime(Canada$Last.Update,format="%Y-%m-%d %H:%M:%S",tz='UTC')
Canada$Std.Time[which(is.na(Canada$Std.Time) == T)] <- strptime(Canada$Last.Update[which(is.na(Canada$Std.Time) == T)],format="%Y-%m-%dT%H:%M:%S",tz='UTC')
Canada$Std.Time[which(is.na(Canada$Std.Time) == T)] <- strptime(Canada$Last.Update[which(is.na(Canada$Std.Time) == T)],format="%m/%d/%y %H:%M",tz='UTC')
Canada$Std.Time[which(is.na(Canada$Std.Time) == T)] <- strptime(Canada$Last.Update[which(is.na(Canada$Std.Time) == T)],format="%m/%d/%Y %H:%M",tz='UTC')

Canada<- Canada[order(Canada$Std.Time),]
Canada$Std.Time<- as.POSIXct(Canada$Std.Time)

Canada$provinces <- as.character(Canada$Province.State)
# Canada$provinces[which(Canada$provinces == ' Montreal, QC')] <- 'Quebec'
# Canada$provinces[which(Canada$provinces == 'Calgary, Alberta')] <- 'Alberta'
# Canada$provinces[which(Canada$provinces == 'Edmonton, Alberta')] <- 'Alberta'
# Canada$provinces[which(Canada$provinces == 'London, ON')] <- 'Ontario'
# Canada$provinces[which(Canada$provinces == 'Toronto, ON')] <- 'Ontario'


provinces<- c('British Columbia','Alberta','Saskatchewan','Manitoba','Ontario','Quebec','New Brunswick','Nova Scotia','Prince Edward Island','Newfoundland and Labrador','Northwest Territories','Yukon','Nunavut','Hubei','Italy')

Canada$provinces[!Canada$provinces %in% provinces]<- 'Other'
Canada<- Canada[-which(Canada$provinces == 'Other'),]
Canada$Confirmed[is.na(Canada$Confirmed) == T] <- 0
Canada$Deaths[is.na(Canada$Deaths) == T] <- 0
Canada$Recovered[is.na(Canada$Recovered) == T] <- 0
# Canada$Total = Canada$Confirmed + Canada$Deaths + Canada$Recovered
Canada$Total = Canada$Confirmed

Canada <- Canada[,c('Std.Time','provinces','Total')]
Canada<- Canada[!duplicated(Canada),]

require(tidyverse)
d2<- Canada %>% 
	group_by(Province=provinces,Day=format(Std.Time,'%Y-%m-%d')) %>%
	summarize_if(is.numeric,max) %>%
	mutate(Change=Total-dplyr::lag(Total,n=7))


d3<- d2
d3$Province <- factor(d3$Province,levels=provinces)

d3$Change[which(d3$Change <= 0)] <- 1
d3$Day<- as.Date(d3$Day)


ggplot(d3, aes(x=Total, y=Change, colour=Province, label=Province)) + geom_line() + geom_point(size=0.5) +
 scale_x_log10() + scale_y_log10() +
 scale_colour_manual(values=c(rep('grey',length(unique(d3$Province))-2),'red','red')) +
 layer(data=d3[which(d3$Day == d3$Day[length(d3$Day)]),], geom='point',stat='identity',position='identity', params=list(colour='red',size=2)) +
 layer(data=d3[which(d3$Day == d3$Day[length(d3$Day)]),], geom='text',stat='identity',position='identity', params=list(colour='black',vjust=0,hjust=0)) +
 coord_fixed(xlim=c(1,max(d3$Total,na.rm=T)*1.5), ylim=c(1,max(d3$Total,na.rm=T)*1.5),ratio=1) +
 labs(x='Total confirmed cases', y='New confirmed cases (in the last week)') + 
 theme_bw() + theme(legend.position='none')


p<- ggplot(d3, aes(x=Total, y=Change, label=Province)) + 
 geom_path(colour='grey') + geom_point(colour='red') + geom_text(hjust=0,vjust=0) + 
 scale_x_log10() + scale_y_log10() +
 coord_fixed(xlim=c(1,max(d3$Total,na.rm=T)*1.5), ylim=c(1,max(d3$Total,na.rm=T)*1.5),ratio=1) +
 theme_bw() + theme(legend.position='none',text=element_text(colour='black')) + 
 labs(x='Total confirmed cases', y='New confirmed cases (in the last week)',title='Date: {frame_along}') +
 transition_reveal(Day)


animate(p, renderer=gifski_renderer()) 
anim_save('/Users/hmvantol/covid_plots/covidCAN.gif')

