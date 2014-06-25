function [R] = recovery_strengths(mu,rho, interference, nSub, nItems)
if nargin < 5
    error('sampling:arg_list','Missing required arguments!')
end 
if ~isvector(mu) || size(mu,1)>1
        error('sampling:means','recovery strength distribution means can only be specified in row vectors')
end

mu=[ mu(1) diff(mu)]; % Calculate means of recovery distributions ( all after the first are mean increases in strength)
means=zeros(nSub,nItems,length(mu),length(interference)); %initalize

for i = 1:length(mu) % fill in array pages with the mean values
    means(:,:,i,:)=mu(i);
end

means(:,end+1,:,:)= reshape(repmat(interference,length(mu)*nSub,1),nSub,1,length(mu),length(interference)); % add the extra-list interference
R=normrnd(means,means*rho); % use array of means to get item strengths sampled from normal distribution
R(R<0)=0; % move any negative strengths up to 0 

end 



% 
% 
% R=zeros(size(sampled,1),size(sampled,2));
% recovery_strengths=normrnd(mu,mu*rho, length(sampled(sampled>=1)),1);
% recovery_strengths(recovery_strengths<0)=0;
% recovery_prob=recovery_strengths./(recovery_strengths + normrnd(interference,interference*rho,length(sampled(sampled>=1)),1));
% recovery_prob(recovery_prob<0)=0;
% recovery_prob(recovery_prob>1)=1;
% R(sampled>=1)=binornd(1,recovery_prob);