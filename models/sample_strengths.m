function [ S ] = sample_strengths(mu,rho, interference, nSub, nItems)
if nargin < 5
    error('sampling:arg_list','Missing required arguments!')
end 
if ~isvector(mu)  || size(mu,1)>1
    error('sampling:means','Item strength distribution means can only be specified in row vectors')
end
mu=[ mu(1) diff(mu)]; % Calculate means of sampling distributions ( all after the first are mean increases in strength)
means=zeros(nSub,nItems,length(mu),length(interference)); %initalize

for i = 1:length(mu) % fill in array pages with the mean values
    means(:,:,i,:)=mu(i);
end

means(:,end+1,:,:)= reshape(repmat(interference,length(mu)*nSub,1),nSub,1,length(mu),length(interference)); % add the extra-list interference
S=normrnd(means,means*rho); % use array of means to get item strengths sampled from normal distribution
S(S<0)=0; % move any negative strengths up to 0 

end 