function [ S ] = sample(mu,rho, interference, nSubjects, nItems, k, condition,varargin)
switch condition
    case 'restudy'
        S = varargin{1};
        increase=normrnd(mu,mu*rho,nSubjects,nItems);
        increase(increase<0)=0;
        S(:,1:end-1)=S(:,1:end-1)+increase;
     case 'study'
        S=[normrnd(mu,mu*rho,nSubjects,nItems) normrnd(interference,interference*rho,nSubjects,1)];
        S(S<0)=0;
     case 'test'
        S=varargin{1};
        recalled=varargin{2};
        increase=normrnd(mu,mu*rho,length(recalled(recalled(1:end-1)==1)));
        S(1:end-1)=S(1:end-1)+increase;
end

S=S./repmat(sum(S,2),1,size(S,2));
S=mnrnd(k,S);%>=1; %500 attempts at sampling items from list, returns the # of time each item was sampled
S(S>=1)=1;
end 