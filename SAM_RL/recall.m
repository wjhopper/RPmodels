function [rec] = recall(S,R,design,k,one_shot)
% determine final test performance, given study/test practice performance

[~,Scn]=find(strcmp(design,'S')); % find study practices
[~,Tcn]=find(strcmp(design,'T')); % find test practices

%% Practice Test 
S_tmp=S(:,:,Tcn-1,:); % grab sampling strengths from before the test practice round occurs
S_tmp=S_tmp./repmat(sum(S_tmp,2),[1,size(S_tmp,2),1,1]);
R_tmp=R(:,:,Tcn-1,:);% grab recovery strengths from before the test practice round occurs
R_tmp=R_tmp./(R_tmp+repmat(R_tmp(:,end,:,:),[1,size(R_tmp,2),1,1]));

S_tmp=permute(S_tmp,[1 3 4 2]); % Rearrange array dimensions (because reshape goes down columns, and you need to move the dimension you need to concatenate along to the second dimension
tmp_size=size(S_tmp); % need to store this size in order to permute things back the way they were!
S_tmp=reshape(S_tmp,prod(tmp_size(1:3)),[]); % reshape because mnrnd needs a 2D matrix

S_tmp=mnrnd(k,S_tmp); % Take k shots at sampling an item
S_tmp(S_tmp>=1)=1; % Score as success(1)/failure (0)
S_tmp=ipermute(reshape(S_tmp,tmp_size),[1 3 4 2]); % put things back into an array 

R_tmp(S_tmp~=1)=0; % if sampling failed (i.e. not equal to 1), of course no chance at recovery

practice_perf=binornd(1,R_tmp); % Take a shot at recovery
practice=mean(squeeze(mean(practice_perf(:,1:end-1,:,:),2))); % mean across subjects and items (Excluding extra-list items in the last column)
no_recovery=(S_tmp==1 & practice_perf==0);
practice_perf=logical(practice_perf);
%% Study-Study
S_tmp=S(:,:,max(Scn),:);
S_tmp=S_tmp./repmat(sum(S_tmp,2),[1,size(S_tmp,2),1,1]);

R_tmp=R(:,:,Tcn-1,:);
R_tmp=R_tmp./(R_tmp+repmat(R_tmp(:,end,:,:),[1,size(R_tmp,2),1,1]));

S_tmp=permute(S_tmp,[1 3 4 2]); % Rearrange array dimensions (because reshape goes down columns, and you need to move the dimension you need to concatenate along to the second dimension
tmp_size=size(S_tmp); % need to store this size in order to permute things back the way they were!
S_tmp=reshape(S_tmp,prod(tmp_size(1:3)),[]); % reshape because mnrnd needs a 2D matrix

S_tmp=mnrnd(k,S_tmp); % Take k shots at sampling an item
S_tmp(S_tmp>=1)=1; % Score as success(1)/failure (0)
S_tmp=ipermute(reshape(S_tmp,tmp_size),[1 3 4 2]); % put things back into an array 

R_tmp(S_tmp~=1)=0;
study_perf=binornd(1,R_tmp);
SS=mean(squeeze(mean(study_perf(:,end-1,:,:),2)));
clear study_perf


%% Study-Test

% get sampling strenths, depending on practice test performance
S_tmp=zeros(size(S,1),size(S,2),1,size(S,4)); %initialize
S_success=S(:,:,max(Tcn),:); % parse out the highest sampling strengths (e.g. after final practice attempt)
S_fail=S(:,:,max(Tcn)-1,:); % parse out the sampling strengths form the next highest (i.e. not correct on test practice)
% Take correct items practice test, and use it to pull out sampling
% stregnths from the 'high' distrbution
S_tmp(repmat(practice_perf(:,:,:,1),[1 1 1 size(R,4)]))=S_success(repmat(practice_perf(:,:,:,1),[1 1 1 size(R,4)]));
% Take incorrect items practice test, and use it to pull out sampling
% stregnths from the 'low' distrbution
S_tmp(repmat(~practice_perf(:,:,:,1),[1 1 1 size(R,4)]))=S_fail(repmat(~practice_perf(:,:,:,1),[1 1 1 size(R,4)]));
S_tmp=S_tmp./repmat(sum(S_tmp,2),[1,size(S_tmp,2),1,1]);% find sampling probability for each item (luce choice rule)

% get recovery strengths, depending on practice performance
R_tmp=zeros(size(R,1),size(R,2),1,size(R,4)); %init
R_success=R(:,:,max(Tcn),:);   % parse out the highest sampling strengths (e.g. after final practice attempt)
R_fail=R(:,:,max(Tcn)-1,:);    % parse out the sampling strengths form the next highest (i.e. not correct on test practice)
% Take correct practice test items, and use it to pull out sampling
% stregnths from the 'high' distrbution
R_tmp(repmat(practice_perf(:,:,:,1),[1 1 1 size(R,4)]))=R_success(repmat(practice_perf(:,:,:,1),[1 1 1 size(R,4)]));
% Take incorrect practice test items, and use it to pull out sampling
% stregnths from the 'low' distrbution
R_tmp(repmat(~practice_perf(:,:,:,1),[1 1 1 size(R,4)]))=R_fail(repmat(~practice_perf(:,:,:,1),[1 1 1 size(R,4)]));
% for T0 (1st page in 4th dimension), if recovery on practice test failed,
% set recovery strength to 0
if one_shot
    R_tmp(no_recovery(:,1:end-1,1,1))=0;
end
R_tmp=R_tmp./(R_tmp+repmat(R_tmp(:,end,:,:),[1,size(R_tmp,2),1,1]));% find recovery probability for each item (luce choice rule)

S_tmp=permute(S_tmp,[1 3 4 2]); % Rearrange array dimensions (because reshape goes down columns, and you need to move the dimension you need to concatenate along to the second dimension
tmp_size=size(S_tmp); % need to store this size in order to permute things back the way they were!
S_tmp=reshape(S_tmp,prod(tmp_size(1:3)),[]); % reshape because mnrnd needs a 2D matrix

S_tmp=mnrnd(k,S_tmp); % Take k shots at sampling an item
S_tmp(S_tmp>=1)=1; % Score as success(1)/failure (0)
S_tmp=ipermute(reshape(S_tmp,tmp_size),[1 3 4 2]); % shape back into array

R_tmp(S_tmp~=1)=0; % if sampling failed, can't recover the item
test_perf=binornd(1,R_tmp);
ST=mean(squeeze(mean(test_perf(:,1:end-1,:,:),2)));

rec=[ SS practice(1) ST];