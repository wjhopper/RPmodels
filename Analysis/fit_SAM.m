function  err = fit_SAM(params)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
% data=[acc on recall practice, acc on 5 min test after study, acc on 5 min
% test after recall practice, acc on 2 day test after study, acc on 2 day
% test after recall practice, acc on 1 week test after study, acc on 1 week
% test after recall practice]
data=[.70,.81,.75,.54,.68,.42,.56];
k=500;
nItems=30;
O_imm=1;
S_one=params(1);
S_two=params(2);
R_one=params(3);
R_two=params(4);
R_correct=params(5);
O_two=params(6);
O_seven=params(7);

p_sample_study=[S_one S_two S_two S_two]./([S_one, S_two S_two S_two]*30  + [O_imm O_imm O_two O_seven]);
p_recover=[R_one R_two R_correct]./([R_one R_two R_correct]+O_imm);


% After initial Study
p_sample_practice=1-((1-p_sample_study(1))^k);
p_recall_practice=p_sample_practice*p_recover(1);
% p_lost=p_sample_practice*(1-p_recover(1));

%After 5 mins
%Studied Items
p_sample_study_imm=1-((1-p_sample_study(2))^k);
p_recall_study_imm=p_sample_study_imm*p_recover(2);

%Tested Items
p_sample_test_correct_imm=S_two/((S_two*floor(p_recall_practice*nItems))...
                                +(S_one*ceil((1-p_recall_practice)*nItems))+O_imm);
%p_sample_test_incorrect_imm=S_one/((S_two*floor(p_recall_practice*nItems))...
%                             +(S_one*ceil((1-p_recall_practice)*nItems))+O_imm);    
%p_sample_test_imm=(floor(p_recall_practice*nItems)/nItems)*(1-((1-p_sample_test_correct_imm)^k))...
%                    + ((ceil((1-p_recall_practice)*nItems)/nItems)*(1-((1-p_sample_test_incorrect_imm)^k)));                         
p_recall_test_imm=((floor(p_recall_practice*nItems)/nItems)*p_recover(3))*(1-((1-p_sample_test_correct_imm)^k)) ;%...
%                  + ((ceil((1-p_recall_practice)*nItems)/nItems)*((1-((1-p_sample_test_incorrect_imm)^k))*0));

% After 2 days
%Studied Items
p_sample_study_two=1-((1-p_sample_study(3))^k);
p_recall_study_two=p_sample_study_two*p_recover(2);
%Tested Items
p_sample_test_correct_two=S_two/((S_two*floor(p_recall_practice*nItems))...
                           +(S_one*ceil((1-p_recall_practice)*nItems))+O_two);
p_recall_test_correct_two=(1-((1-p_sample_test_correct_two)^k))*p_recover(3);
p_sample_test_incorrect_two=S_one/((S_two*floor(p_recall_practice*nItems))...
                             +(S_one*ceil((1-p_recall_practice)*nItems))+O_two);
p_recall_test_incorrect_two=(1-((1-p_sample_test_incorrect_two)^k))*p_recover(1);
p_recall_test_two= ((floor(p_recall_practice*nItems)/nItems*p_recall_test_correct_two))...
                    + ((ceil((1-p_recall_practice)*nItems)/nItems)*p_recall_test_incorrect_two);
               

%After 7 days
%Studied Itesm
p_sample_study_seven=1-((1-p_sample_study(4))^k);
p_recall_study_seven=p_sample_study_seven*p_recover(2);
%Tested Items
p_sample_test_correct_seven=S_two/((S_two*floor(p_recall_practice*nItems))...
                           +(S_one*ceil((1-p_recall_practice)*nItems))+O_seven);
p_recall_test_correct_seven=(1-((1-p_sample_test_correct_seven)^k))*p_recover(3);
p_sample_test_incorrect_seven=S_one/((S_two*floor(p_recall_practice*nItems))...
                             +(S_one*ceil((1-p_recall_practice)*nItems))+O_seven);
p_recall_test_incorrect_seven=(1-((1-p_sample_test_incorrect_seven)^k))*p_recover(1);
p_recall_test_seven= ((floor(p_recall_practice*nItems)/nItems*p_recall_test_correct_seven))...
                    + ((ceil((1-p_recall_practice)*nItems)/nItems)*p_recall_test_incorrect_seven);
                
                

pred_tot=[p_recall_practice p_recall_study_imm p_recall_test_imm ...
    p_recall_study_two p_recall_test_two p_recall_study_seven p_recall_test_seven];
%p_sample=[p_sample_one p_sample_study_imm

Lu=(data.*log(data))+((1-data).*log(1-data));
Lc=(data.*log(pred_tot))+((1-data).*log(1-pred_tot));
err=-sum((2*nItems*(Lc-Lu)));

if (S_one <=0 || S_two <= 0 || R_one <= 0 || R_correct <= 0 || O_two <= 0 || O_seven <= 0)
    err=1000000;
end

if S_two < S_one;
   err=1000000;
end 

if R_two < R_one;
   err=1000000;
end 

if R_correct < R_one;
   err=1000000;
end 

figure(1);
hold off
plot(data);
hold on
plot(pred_tot,'r');


