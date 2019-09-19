excl<-"NYJ"
j<-0
i<-0
return<-list(1,2,3,4,5,6,7,8)
add<-data.frame(optimal=list(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
names(add)<-header
temp<-data.frame(optimal=return)
Pos<-c("WR","RB","QB","TE","K","D")
low<-c("wr","rb","qb","te","kicker","defense")
col<-c("isWR","isRB","isQB","isTE","isK","isDEF")
header<-c("Player","Cost","Team","Pos","Opp","V1","V2","Value","isWR","isRB","isQB","isTE","isK","isDEF")
 for(i in seq(1,6)){
	site<-paste("https://rotogrinders.com/projected-stats/nfl-",low[i],".csv?site=fanduel",sep="")
	fpath<-paste("~/Sports stats/",low[i],sep="")
	write(read_html(site) %>% 
	html_node("body") %>%
	html_text(),file=fpath)
	temp<-read.csv(fpath,header=FALSE)
	temp<-temp[seq(1,NROW(temp)-1),]
	for(k in seq(1,6)){temp<-cbind(temp,seq(1,NROW(temp)))}
	names(temp)<-header
	add<-rbind(add,temp)
 }
for(i in seq(1,6)){
	for(j in seq(1,NROW(add))) {
		add[j,col[i]]<-0
		if (add[j,'Pos']==Pos[i]) {add[j,col[i]]<-1}
		if (add[j,'Cost']==0) {
			add[j,'Value']<-0
			add[j,'Cost']<-5000}
		if (add[j,'Team']==excl) {add[j,'Value'] <- 0}
		if (add[j,'Opp']==excl) {add[j,'Value'] <- 0}
		}
	}
Mock.NFL.data <- add[seq(2,NROW(add)),]
Mock.NFL.data[[2]]<-as.numeric(Mock.NFL.data[[2]])
Mock.NFL.data[[8]]<-as.numeric(Mock.NFL.data[[8]])
numPlay<-9
aug<-0
tmpInd1<-0
tmpInd2<-0
tmpMin<-0
tmpMinX<-numeric(5)
temp1<-0
temp2<-0
quit<-0
aug<-0
max<-numeric(5)
total<-0
minMax<-0
return<-list(1,2,3,4,5)
df<-data.frame(optimal=return)
vals<-data.frame(optimal=return)
p<-numeric(numPlay)
for(j in seq(1,numPlay)) {
  count<-1
  model<-make.lp(0,NROW(Mock.NFL.data))
  set.objfn(model,c(Mock.NFL.data[['Value']]))
  add.constraint(model,c(Mock.NFL.data[['Cost']]),"<=",60000)
  add.constraint(model,numeric(NROW(Mock.NFL.data))+1,"=",numPlay)
  add.constraint(model,c(Mock.NFL.data[['isWR']]),"=",3)
  add.constraint(model,c(Mock.NFL.data[['isRB']]),"=",2)
  add.constraint(model,c(Mock.NFL.data[['isQB']]),"=",1)
  add.constraint(model,c(Mock.NFL.data[['isTE']]),"=",1)
  add.constraint(model,c(Mock.NFL.data[['isK']]),"=",1)
  add.constraint(model,c(Mock.NFL.data[['isDEF']]),"=",1)
  set.type(model,seq(1,NROW(Mock.NFL.data)),"binary")
  lp.control(model,sense='max')
  solve(model)
  player<-get.variables(model)
  total<-get.objective(model)
  for(i in seq(1,NROW(Mock.NFL.data))){
    if(player[i]==1) {    
      p[count]<- i
      count<-count+1
      }
    }
  quit<-0
  if(total>minMax){
    for(t in seq(1,5)){
      if(quit==0){  
        if(max[t]<total){
          for(a in seq(1,numPlay)){
            Mock.NFL.data[p[a],'Value']<-Mock.NFL.data[p[a],'Value']*.95
            if(tmpMinX[t]>0){
            Mock.NFL.data[vals[a,tmpMinX[t]],'Value']<-Mock.NFL.data[vals[a,tmpMinX[t]],'Value']/.95}
          }
          for(l in seq(5,t)){
            max[min(l+1,5)]<-max[l]
            return[[min(l+1,5)]]<-return[[l]]
            tmpMinX[min(l+1,5)]<-tmpMinX[l]
            }
          max[t]<-total
          tmpMinX[t]<-j
          minMax<-min(max)
          quit<-1
          return[[t]]<-Mock.NFL.data[p,'Player']
          }
        }
      }
    }
  if(aug==1){
    df<-cbind(df,v=return)
    vals<-cbind(vals,v=p)
    }
  if(aug==0){
    vals<-cbind('Optimal'=p)
    }
    aug<-1
}
return

