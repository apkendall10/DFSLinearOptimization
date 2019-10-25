getData <-function(source="fanduel",excl=list("	")){
	#initialize variables
	defense <- "D"
	if(source=="draftkings"){
		defense <- "DST"
	}
	data<-data.frame(optimal=list(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
	temp<-data.frame(optimal=list(1,2,3,4,5,6,7,8))
	col<-c("isWR","isRB","isQB","isTE","isK","isDEF")
	pos<-c("WR","RB","QB","TE","K",defense)
	positionList<-c("wr","rb","qb","te","kicker","defense")
	header<-c("Player","Cost","Team","Pos","Opp","V1","V2","Value","isWR","isRB","isQB","isTE","isK","isDEF")
	names(data)<-header
	#grab data from rotogrinders for each position and store in data
	for(position in positionList){
		site<-paste("https://rotogrinders.com/projected-stats/nfl-",position,".csv?site=",source,sep="")
		#https://rotogrinders.com/projected-stats/nfl-rb.csv?site=draftkings
		fpath<-paste("~/Documents/",position,".csv",sep="")
		temp <- read.csv(site, header=FALSE)
		#write.csv(temp, file = fpath)
		#temp<-temp[seq(1,NROW(temp)-1),]
		#add extra columns to hold positional attributes
		for(k in seq(1,6)){temp<-cbind(temp,seq(1,NROW(temp)))}
		names(temp)<-header
		data<-rbind(data,temp)
	}
	#fill in posoitional attributes for each player
	for(i in seq(1,6)){
		for(j in seq(1,NROW(data))) {
			data[j,col[i]]<-0
			if (data[j,'Pos']==pos[i]) {
				data[j,col[i]]<-1
				}
			#handle corner cases in data
			if (data[j,'Cost']==0) {
				data[j,'Value']<-0
				data[j,'Cost']<-5000
			}
			#handle exclusions if we want to play games that don't involve all days
			if (any(excl==data[j,'Team'])) {data[j,'Value'] <- 0}
			if (any(excl == data[j,'Opp'])) {data[j,'Value'] <- 0}
		}
	}
	#do some additional formatting
	data <- data[seq(2,NROW(data)),]
	data[[2]]<-as.numeric(data[[2]])
	data[[8]]<-as.numeric(data[[8]])
	return(data)
}