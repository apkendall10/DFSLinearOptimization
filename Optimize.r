library(lpSolveAPI)
source("GetData.r")

#load data and initialize parameters
nflData <- data.frame(optimal=list(1,2,3,4,5,6,7,8,9,10,11,12,13,14))
nflData <- getData()
numPlay<-9
return<-list(1,2,3,4,5)
playerIndex<-numeric(numPlay)
#Run model 5 times to find 5 best linesups
for(j in seq(1,5)) {
    count<-1
    #make model
    model<-make.lp(0,NROW(nflData))
    set.objfn(model,c(nflData[['Value']]))
    add.constraint(model,c(nflData[['Cost']]),"<=",60000)
    add.constraint(model,numeric(NROW(nflData))+1,"=",numPlay)
    add.constraint(model,c(nflData[['isWR']]),"=",3)
    add.constraint(model,c(nflData[['isRB']]),"=",2)
    add.constraint(model,c(nflData[['isQB']]),"=",1)
    add.constraint(model,c(nflData[['isTE']]),"=",1)
    add.constraint(model,c(nflData[['isK']]),"=",1)
    add.constraint(model,c(nflData[['isDEF']]),"=",1)
    set.type(model,seq(1,NROW(nflData)),"binary")
    lp.control(model,sense='max')
    #solve model and store best lineup and values
    solve(model)
    playerList<-get.variables(model)
    total<-get.objective(model)
    return[[j]]<-nflData[playerIndex,'Player']
    #decrease selected player values to diversify next selection
    for(i in seq(1,NROW(nflData))){
        if(playerList[i]==1) {    
        playerIndex[count]<- i
        count<-count+1
        nflData[i,'Value']<-nflData[i,'Value']*.97
        }
    }
}

#print out results
return
