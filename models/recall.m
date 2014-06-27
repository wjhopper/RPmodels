function [rec] = recall(S,R,design,k)
% determine final test performance, given study/test practice performance

S=S./repmat(sum(S,2),[1,size(S,2),1,1]);
R=R./(R+repmat(R(:,end,:,:),[1,size(R,2),1,1]));
S_tmp=permute(S,[1 3 4 2]);
S=reshape(S_tmp,(size(S,1)*size(S,3)*size(S,4)),[]);
% R_tmp=permute(R2,[1 3 4 2]);
% R2=reshape(R_tmp,(size(R2,1)*size(R2,3)*size(R2,4)),[]);
S=mnrnd(k,S); %500 attempts at sampling items from list, returns the # of time each item was sampled
S(S>=1)=1; 
S=ipermute(reshape(S,size(S_tmp)),[1 3 4 2]);
clear S_tmp
[~,Scn]=find(strcmp(design,'S')); % find study practices
[~,Tcn]=find(strcmp(design,'T')); % find test practices

%% Practice Test 

R2=zeros(size(S(:,:,unique(Tcn)-1,:)),'single');
R2(S(:,:,(unique(Tcn)-1),:)==1)= R(S(:,:,unique(Tcn)-1,:)==1);
practice_perf=binornd(1,R2);
practice=squeeze(mean(practice_perf(:,end-1,:)));

%% Study-Study
% R2=zeros(size(S(:,:,max(Scn),:)),'single');
% R2(S(:,:,max(Scn),:)==1) = R(S(:,:,max(Scn),:)==1);
R2=zeros(size(S),'single');
R2(S==1)=R(S==1);
R2=R2(:,:,1:(max(Scn)-1),:);
study_perf=binornd(1,R2);
SS=squeeze(mean(study_perf(:,end-1,:)));
clear study_perf

%% Study-Test
R2=zeros(size(practice_perf),'single');
R3=R(:,:,Tcn,:);
R4=R(:,:,Tcn-1,:);
R2(practice_perf==1)=R3(practice_perf==1);
R2(practice_perf==0)=R4(practice_perf==0);
test_perf=binornd(1,R2);
ST=squeeze(mean(test_perf(:,end-1,:)));

rec=[practice(1) SS' ST'];