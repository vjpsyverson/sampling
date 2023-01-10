options(stringsAsFactors = F)
require(jsonlite);require(httr);require(paleobioDB);require(beepr)

conts<-read.csv("https://paleobiodb.org/data1.2/config.txt?show=continents")
conts<-rbind(conts,c("con","All","NA"))
cont.abbr<-conts$continent_code
data<-vector(mode="list",length(cont.abbr))
taxon<-"Brachiopoda"
for (i in 1:(length(cont.abbr)-1)){
  call<-paste0("https://paleobiodb.org/data1.2/occs/quickdiv.csv?base_name=",taxon,"&continent=",cont.abbr[i],"&count=genera",collapse="")
  data[[i]]<-as.data.frame(read.csv(call))
}
call<-paste0("https://paleobiodb.org/data1.2/occs/quickdiv.csv?base_name=",taxon,"&count=genera")
data[[length(cont.abbr)]]<-as.data.frame(read.csv(call))
names(data)<-conts$continent_name
contColors<-c(rainbow(length(cont.abbr)-1),"black");names(contColors)<-conts$continent_name

#occs not logged - wow @ NOA, also even EUR is noticeably different esp. in timing of Devonian peak and extinction
plot(1,1,type="n",
     xlim=c(max(apply(do.call(rbind.data.frame,data)[c("max_ma","min_ma")],1,mean),na.rm=T),250),
     ylim=range(do.call(rbind.data.frame,data)$n_occs,na.rm=T),
     xlab="time(Ma)",ylab="occurrences",
     main=paste0(taxon," occurrences"))
for(i in 1:length(cont.abbr)){  time<-apply(data[i][[1]][c("max_ma","min_ma")],1,mean,na.rm=T);  points(cbind(time,data[i][[1]]["n_occs"]),type="l",xlim=rev(range(time)),col=contColors[i])};  legend("topright",lty=1,toupper(names(contColors)[c(2,1,3:length(cont.abbr))]),col=contColors,bty="n",cex=0.6)

##occs logged - AUS/SOA (Gondwana?) has an Early Permian peak; ASI has a late Ord/Sil start and a full stage late end-Dev extinction
plot(1,1,type="n",log="y",
     xlim=c(max(apply(do.call(rbind.data.frame,data)[c("max_ma","min_ma")],1,mean),na.rm=T),250),
     ylim=range(do.call(rbind.data.frame,data)$n_occs,na.rm=T)+1,
     xlab="time(Ma)",ylab="log(occurrences)",
     main=paste0(taxon," occurrences (logged)"))
for(i in 1:length(cont.abbr)){time<-apply(data[i][[1]][c("max_ma","min_ma")],1,mean);points(cbind(time,data[i][[1]]["n_occs"]+1),type="l",xlim=rev(range(time)),col=contColors[i])}  ;legend("topleft",lty=1,toupper(names(contColors)[c(2,1,3:length(cont.abbr))]),col=contColors,bty="n",cex=0.7)

##quickdiv not logged - basically the signal is all NOA except for the Sil-Dev of EUR. the AUS diversity in the Permian probably reflects Timor? look at ASI in the Famennian and Tournaisian, it does the opposite of the global signal!
plot(1,1,type="n",
     xlim=c(max(apply(do.call(rbind.data.frame,data)[c("max_ma","min_ma")],1,mean),na.rm=T),250),
     ylim=range(do.call(rbind.data.frame,data)$sampled_in_bin,na.rm=T),xlab="time(Ma)",ylab="genera",
     main=paste0(taxon," diversity"))
for(i in 1:length(cont.abbr)){  time<-apply(data[i][[1]][c("max_ma","min_ma")],1,mean);  points(cbind(time,data[i][[1]]["sampled_in_bin"]),type="l",xlim=rev(range(time)),col=contColors[i])};  legend("topleft",lty=1,toupper(names(contColors)[c(2,1,3:length(cont.abbr))]),col=contColors,bty="n",cex=0.6)
