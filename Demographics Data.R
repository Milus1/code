#Demographics
dpath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/demographics"
data_demo <- ldply(list.files(path=dpath, full.names=TRUE), read.csv, header=TRUE)
View(data_demo)

data_demo<-data_demo[which((data_demo$ID %in% data_pc1$ID)==TRUE),] #extract participants who didn't complete the experiment by finding mismatch
summary(data_demo[,-1])
sapply(data_demo[,-1],mean)
sapply(data_demo[,-1],sd)

#By Gender
Femaledemo<-data_demo[which(data_demo$Gender==1),names(data_demo) %in% c(colnames(data_demo))]
Maledemo<-data_demo[which(data_demo$Gender==2),names(data_demo) %in% c(colnames(data_demo))]
Otherdemo<-data_demo[which(data_demo$Gender==3),names(data_demo) %in% c(colnames(data_demo))] #the only participant has been extracted

f<-function(x){
  list(mean(x),sd(x),length(x))
}

Femaled<-sapply(Femaledemo[,-1],f)
Maled<-sapply(Maledemo[,-1],f)
list(Femaled,Maled)