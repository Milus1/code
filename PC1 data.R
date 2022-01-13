#### Perceived controllability ####
library(plyr)
pc1path <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/perceived_control1" 
data_pc1 <- ldply(list.files(path=pc1path, full.names=TRUE), read.csv, header=TRUE)
length(unique(data_pc1$ID)) #check whether the participant lists are complete
rewardfreq<-(data_pc1$X.10guess+data_pc1$X.11guess)/100 #the total expected frequency of reward
data_pc1<-cbind(data_pc1,rewardfreq) #combine

exppath <- "C:/Users/ASUS/Documents/3rd Year/Thesis Writing/exploration" 
data_exp <- ldply(list.files(path=exppath, full.names=TRUE), read.csv, header=TRUE)
MQpath <-"C:/Users/ASUS/Documents/3rd Year/Thesis Writing/MQ" 
data_MQ <- ldply(list.files(path=MQpath, full.names=TRUE), read.csv, header=TRUE)
data_exp<-data_exp[which((data_exp$ID %in% data_MQ$ID)==TRUE),] #extract participants who didn't complete the experiment
data_pc1<- data_pc1[which((data_pc1$ID %in% data_MQ$ID)==TRUE),] #remove participants

Conditions<-rep(data_exp$Control.Condition,each=16) #for each unique ID
Reward_Frequency <- rep(data_exp$Reward.Frequency,each=16)
data_pc1<-cbind(data_pc1,Conditions,Reward_Frequency) #add conditions of participants to data frame
data_pc1$Conditions[data_pc1$Conditions == 'CTRL']<-1
data_pc1$Conditions[data_pc1$Conditions == 'YOK']<-0
View(data_pc1)

#Calculate variance between 4 sequential trials
varbtw<- numeric()
for (i in 1:(nrow(data_pc1)/4)){
  varbtw[i]<- var(data_pc1[(4*i-3):(4*i),9])
}

condit<-numeric()
for(i in 1:(nrow(data_pc1)/4)){
  condit[i]<-data_pc1[4*i,10]
}

prob<-numeric()
for(i in 1:(nrow(data_pc1)/4)){
  prob[i]<-data_pc1[4*i,11]
}


Trials<-rep(1:4,times=45) #Trial numbers

Vardata <- data.frame(var_btw_reward=varbtw,Control=condit,Reward=prob,Trials)
#divide by blocks
Block1<- data.frame(Vardata[which(Vardata$Trials=='1'),])
Block1<-Block1[,c(2,3,4,1)]
Block2<- data.frame(Block2_var=Vardata$var_btw_reward[Vardata$Trials=='2'])
Block3<- data.frame(Block3_var=Vardata$var_btw_reward[Vardata$Trials=='3'])
Block4<- data.frame(Block4_var=Vardata$var_btw_reward[Vardata$Trials=='4'])
Vardata2<- data.frame(Block1[-3],Block2,Block3,Block4)
View(Vardata2)

#means of PC1 by condition & reward frequency & Block
ps<- c(0.1,0.2,0.4,1)

means_PC1 <- data.frame()
for(m in 1:2) {
  for(j in 1:4) {
    for(i in 1:4) {
      
      means_PC1[j+(m-1)*4, i] <- mean(Vardata2[which(Vardata2[2] == ps[j] & Vardata2[1] == (m-1)), 2+i])
      
    }
  }
}
colnames(means_PC1) <- c(paste("Block", 1:4))
rownames(means_PC1) <- c(paste("Control", ps), paste("Yoked", ps))
View(means_PC1)

Half1<-rowMeans(means[,c('Block 1', 'Block 2')], na.rm=TRUE)
Half2<-rowMeans(means[,c('Block 3','Block 4')],na.rm=TRUE)
meanshalf_PC1<-cbind(means_PC1,Half1,Half2)
#each blocks + half
View(meanshalf_PC1)

Half1_0.1 <- c(meanshalf_PC1[1,5],meanshalf_PC1[5,5])
Half1_0.2 <- c(meanshalf_PC1[2,5],meanshalf_PC1[6,5])
Half1_0.4 <- c(meanshalf_PC1[3,5],meanshalf_PC1[7,5])
Half1_1 <- c(meanshalf_PC1[4,5],meanshalf_PC1[8,5])

Half2_0.1 <- c(meanshalf_PC1[1,6],meanshalf_PC1[5,6])
Half2_0.2 <- c(meanshalf_PC1[2,6],meanshalf_PC1[6,6])
Half2_0.4 <- c(meanshalf_PC1[3,6],meanshalf_PC1[7,6])
Half2_1 <- c(meanshalf_PC1[4,6],meanshalf_PC1[8,6])

par(mfrow = c(2,1))

#Plot for block 1+2 Perceived Controllabliity
Half1_PC1plot<- data.frame(Half1_0.1,Half1_0.2,Half1_0.4,Half1_1)
names(Half1_PC1plot) <- c("p=0.1","p=0.2","p=0.4","p=1")
barplot(height=as.matrix(Half1_PC1plot),main="Block 1+2",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))
#Plot for block 3+4 Perceived Controllabliity
Half2_PC1plot<- data.frame(Half2_0.1,Half2_0.2,Half2_0.4,Half2_1)
names(Half2_PC1plot) <- c("p=0.1","p=0.2","p=0.4","p=1")
barplot(height=as.matrix(Half2_PC1plot),main="Block 3+4",ylab="Average Perceived Controllability",beside=TRUE, col=c("red","blue"))
legend("topleft",c("With-Control","Yoked"),cex=1.0,bty="n",fill=c("red","blue"))
