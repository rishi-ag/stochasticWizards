

plot.path<-function(target,state,real_path,sim_loss=NULL,real_loss=NULL,main="Path"){
    minx<-min(target[,1],state[,1],real_path[,1])
    maxx<-max(target[,1],state[,1],real_path[,1])
    miny<-min(target[,2],state[,2],real_path[,2])
    maxy<-max(target[,2],state[,2],real_path[,2])
    
    plot(target,type="l",col="blue",main=main,
         xlim=c(minx,maxx),
         ylim=c(miny,maxy),
         xlab="X coordinates",ylab="Y coordinates")
    lines(state,type="l",col="red")
    lines(real_path,type="l",col="green")
    legend("bottomright",legend=c("target","simulated","real"),lty=1,
           bg="#FFFFFFAA",col=c("blue","red","green"))
    text(585900,4517000,paste0('Real loss: ',round(real_loss),'\nSim loss: ',round(sim_loss)),cex=0.6,pos=4)
    
}

plot.bars<-function(sim_loss,real_loss,type,main="Losses"){
    require(ggplot2)
    n<-length(sim_loss)
    df<-data.frame(loss=c(sim_loss,real_loss),case=rep(c("simulated","real"),each=n),
                   wind_type=rep(type,2))
    
    qplot(factor(wind_type,as.character(wind_type)),data=df,geom="bar",fill=case,weight=loss,position="dodge",
          main = main, xlab="Wind model",ylab="")
}