function [R] = recover(mu,rho, interference,sampled, ~)
R=zeros(size(sampled,1),size(sampled,2));
recovery_strengths=normrnd(mu,mu*rho, length(sampled(sampled>=1)),1);
recovery_strengths(recovery_strengths<0)=0;
recovery_prob=recovery_strengths./(recovery_strengths + normrnd(interference,interference*rho,length(sampled(sampled>=1)),1));
recovery_prob(recovery_prob<0)=0;
recovery_prob(recovery_prob>1)=1;
R(sampled>=1)=binornd(1,recovery_prob);