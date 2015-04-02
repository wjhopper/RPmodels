SAMRI <- function (params=data.frame(S1=0,S2=0,R=0,Rcor=0,O2=0, O7=0, k=0,nitems=0),
                   t=seq(1,5,.1),one_shot='never',predict = TRUE) {

  p_sample_study=data.frame('sampling_initStudy'=params$S1/((params$S1*30)+t),
                            'sampling_studyPrac'=params$S2/((params$S2*30)+t))
  p_recover=data.frame('recovery_Study'=(params$R/((params$R)+t)),
                       'recovery_testPrac'=(params$Rcor/((params$Rcor)+t)))
  
  # Study condition
#   p_sample_practice=1-((1-p_sample_study[1,'sampling_initStudy'])^k)
#   p_recall_practice=p_sample_practice*p_recover[1,'recovery_Study']
# 
#   p_sample_study_final=1-((1-p_sample_study[,2])^k)
#   p_recall_study_final=p_sample_study_final*p_recover[,1]
#   
  percentSampledOneStudy <-1-((1-p_sample_study$sampling_initStudy)^params$k)
  acc_OneStudy<-percentSampledOneStudy*p_recover$recovery_Study # row 1 is practice test performance!
  
  percentSampledTwoStudy <-1-((1-p_sample_study$sampling_studyPrac)^params$k)
  acc_TwoStudy<-percentSampledTwoStudy*p_recover$recovery_Study


  #Test Condition
  #Correct Items
  p_sample_test_correct=params$S2/((params$S2*acc_OneStudy[1]*params$nItems)
                                 +(params$S1*(1-acc_OneStudy[1])*params$nItems)+t);
  p_recall_test_correct=(1-((1-p_sample_test_correct))^params$k)*p_recover[,'recovery_testPrac']
  #Incorrect Items
  p_sample_test_incorrect=params$S1/((params$S2*acc_OneStudy[1]*params$nItems)
                                     +(params$S1*(1-acc_OneStudy[1])*params$nItems)+t);
  p_recall_test_incorrect=(1-((1-p_sample_test_incorrect))^params$k)*p_recover[,'recovery_Study']

  #Recall with same context as practice 
  acc_Test_same_context=(p_recall_test_correct*acc_OneStudy[1]) + (p_recall_test_incorrect*(1-percentSampledOneStudy[1]));

  #Recall with different context than practice
  acc_Test_diff_context=(p_recall_test_correct*acc_OneStudy[1]) + (p_recall_test_incorrect*(1-acc_OneStudy[1]));

  if (one_shot== 'always') { # one shot always on
    return(preds = data.frame(time=t,OneStudy=acc_OneStudy, TwoStudy=acc_TwoStudy, 
                              Test=acc_Test_same_context, one_shot=rep(one_shot,length(t))))
    
    } else if (one_shot == 'never') { # one shot off
      return(preds = data.frame(time=t,OneStudy=acc_OneStudy, TwoStudy=acc_TwoStudy, 
                                Test=acc_Test_diff_context, one_shot=rep(one_shot,length(t))))

    } else if (one_shot =="mixed") {
      # pick 20th time percentile as arbirary context change point
      if (predict) {
        change_val <- which(t== quantile(t,.2))
      } else {
        change_val <- 1
      }
      acc_Test_mixed = c(acc_Test_same_context[1:change_val], acc_Test_diff_context[-c(change_val:0)])
      return(preds = data.frame(time=t,OneStudy=acc_OneStudy, 
                                TwoStudy=acc_TwoStudy, Test=acc_Test_mixed, one_shot=rep(one_shot,length(t))))
    }
}
