O<-seq(1,10,.1)
k=500
nItems=30

params<-read.csv(file.choose(),header=F,
                 col.names=c('S1','S2','R1','R2','O2','O7'))
good<-unlist(params[1,])
bad<-unlist(params[2,])
data<-data.frame(acc=c(.70,.81,.75,.54,.68,.42,.56),
                 time=c(1,1,1,2.9828,2.9828,4.499,4.499),
                 group=c('test','study','test','study','test','study','test')
                )
## good version, without second chance
par(mar=c(5,5,3,1))
plot(data[data$group=='test','time'][2:4],data[data$group=='test','acc'][2:4],
     lwd=3,pch=4,col='red',ylim=c(.35,.9),xlim=c(1,10),
     ylab="Recall Accuracy",xlab="Interference (O) / Retention Interval (days)",
     cex.lab=2.5,cex.main=2,main="Observed Data (Roediger & Karpicke, 2006) & SAM-RL Model Predictions")
points(data[data$group=='test','time'][1],data[data$group=='test','acc'][1],
       lwd=3,pch=2,col='blue')
points(data[data$group=='study','time'],data[data$group=='study','acc'],
       lwd=3,pch=5,col='green')
legend("topright",legend=c("Practice Test","Final Test (Restudy)","Final Test (Testing)","Model Predictions (No Extra Practice)","Model Predictions (Restudy)","Model Predictions (Testing)")
       ,col=c('blue','green','red','blue','green','red'),lwd=2,pch=c(2,5,4,NA,NA,NA),lty=c(NA,NA,NA,2,2,2),cex=2)

p_sample_study=data.frame('S1'=good[1]/((good[1]*30)+O),'S2'=good[2]/((good[2]*30)+O))
p_recover=data.frame('R1'=good[3]/((good[3])+O),'R2'=good[4]/((good[4])+O))


p_sample_practice=1-((1-p_sample_study[1,1])^k)
p_recall_practice=p_sample_practice*p_recover[1,1]
practice_sampling <-1-((1-p_sample_study[,1])^k)
practice_forgetting<-practice_sampling*p_recover[,1]


p_sample_study_final=1-((1-p_sample_study[,2])^k)
p_recall_study_final=p_sample_study_final*p_recover[,1]

#Test Condition
#Correct Items
p_sample_test_correct=good[2]/((good[2]*p_recall_practice*nItems)
                              +(good[1]*(1-p_recall_practice)*nItems)+O);
p_recall_test_correct=(1-((1-p_sample_test_correct))^k)*p_recover[,2]

#Incorrect Items
p_sample_test_incorrect=good[1]/((good[2]*(p_recall_practice*nItems))
                                +(good[1]*(1-p_recall_practice)*nItems)+O)
p_recall_test_incorrect=(1-((1-p_sample_test_incorrect))^k)*p_recover[,1]
#Recall on immediate
p_recall_test_imm=(p_recall_test_correct*p_recall_practice) + (p_recall_test_incorrect*(1-p_sample_practice));
#Recall on delayed tests
p_recall_test_delay=(p_recall_test_correct*p_recall_practice) + (p_recall_test_incorrect*(1-p_recall_practice));

#plot the predictions
lines(O,c(p_recall_test_imm[1:2],p_recall_test_delay[3:91]),lty=2,lwd=2,col='red')
lines(O,p_recall_study_final,lty=2,lwd=2,col='green')
lines(O,practice_forgetting,lty=2,lwd=2,col='blue')



### Bad version, with second chance
O<-seq(1,5,.1)
data<-data.frame(acc=c(.70,.81,.75,.54,.68,.42,.56),
                 time=c(1,1,1,1.651246,1.651246,2.297341,2.297341),
                 group=c('test','study','test','study','test','study','test')
                )
plot(data[data$group=='test','time'][2:4],data[data$group=='test','acc'][2:4],
     lwd=3,pch=4,col='red',ylim=c(.35,.9),xlim=c(1,5),
     ylab="Recall Accuracy",xlab="Interference (O) / Retention Interval (days)",
     cex.lab=2.5,cex.main=2,main="Observed Data (Roediger & Karpicke, 2006) & SAM-RL Model Predictions")
points(data[data$group=='test','time'][1],data[data$group=='test','acc'][1],
       lwd=3,pch=2,col='blue')
points(data[data$group=='study','time'],data[data$group=='study','acc'],
       lwd=3,pch=5,col='green')
legend("topright",legend=c("Practice Test","Final Test (Restudy)","Final Test (Testing)","Model Predictions (No Extra Practice)","Model Predictions (Restudy)","Model Predictions (Testing)")
       ,col=c('blue','green','red','blue','green','red'),lwd=2,pch=c(2,5,4,NA,NA,NA),lty=c(NA,NA,NA,2,2,2),cex=2)

p_sample_study=data.frame('S1'=bad[1]/((bad[1]*30)+O),'S2'=bad[2]/((bad[2]*30)+O))
p_recover=data.frame('R1'=bad[3]/((bad[3])+O),'R2'=bad[4]/((bad[4])+O))


p_sample_practice=1-((1-p_sample_study[1,1])^k)
p_recall_practice=p_sample_practice*p_recover[1,1]
practice_sampling <-1-((1-p_sample_study[,1])^k)
practice_forgetting<-practice_sampling*p_recover[,1]

p_sample_study_final=1-((1-p_sample_study[,2])^k)
p_recall_study_final=p_sample_study_final*p_recover[,1]

#Test Condition
#Correct Items
p_sample_test_correct=bad[2]/((bad[2]*p_recall_practice*nItems)
                               +(bad[1]*(1-p_recall_practice)*nItems)+O);
p_recall_test_correct=(1-((1-p_sample_test_correct))^k)*p_recover[,2]

#Incorrect Items
p_sample_test_incorrect=bad[1]/((bad[2]*(p_recall_practice*nItems))
                                 +(bad[1]*(1-p_recall_practice)*nItems)+O)
p_recall_test_incorrect=(1-((1-p_sample_test_incorrect))^k)*p_recover[,1]

#Recall on all tests 
p_recall_test=(p_recall_test_correct*p_recall_practice) + (p_recall_test_incorrect*(1-p_recall_practice));

#plot the predictions
lines(O,p_recall_test,lty=2,lwd=2,col='red')
lines(O,p_recall_study_final,lty=2,lwd=2,col='green')
lines(O,practice_forgetting,lty=2,lwd=2,col='blue')
