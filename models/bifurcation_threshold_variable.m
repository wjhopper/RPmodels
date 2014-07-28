function recall_threshold_variable

S1=.5;
S2=1;
Stest=2.6;
O2=.9;
O7=1.2;
theta=0; % recall threshold
sigma=1;  % threshold variability 
rho=1;

count=1000;
    
lx=-4;  % smallest value
mx=4;   % largest value
rx=(mx-lx)./(count-1);  % step size
x=lx:rx:mx; % vector of steps

prac=sum(normpdf(x,S1,rho).*normcdf(x,theta,sigma)).*rx 
study_imm=sum(normpdf(x,S2,rho).*normcdf(x,theta,sigma)).*rx 
study_2=sum(normpdf(x,S2-O2,rho).*normcdf(x,theta,sigma)).*rx 
study_3=sum(normpdf(x,S2-O7,rho).*normcdf(x,theta,sigma)).*rx 

test_imm=sum(normpdf(x,Stest,rho).*normcdf(x,theta,sigma)).*rx*prac
test_2=sum(normpdf(x,Stest-O2,rho).*normcdf(x,theta,sigma)).*rx.*prac
test_2=sum(normpdf(x,Stest-O7,rho).*normcdf(x,theta,sigma)).*rx.*prac