function bifurcation(variance)
% data=[acc on 5 min test after study,  acc on 2 day test after study,  acc on 1 week test after study, 
    %       acc on recall practice,  acc on 5 min test after recall practice,
    %       acc on 2 day test after recall practice, acc on 1 week test after recall practice] 
data=[ .81, .54, .42, .70,  .75, .68, .56];

S1=.5;
S2=1;
Stest=2.6;
O2=.9;
O7=1.2;
theta=0;
rho=1;
sigma=.01;

if nargin == 1 && strcmp(variance,'variance')
    [params,chi_square,exitflag,output] = fminsearch(@bifurcate_variability,[S1 S2 Stest O2 O7 rho sigma])
else
    [params,chi_square,exitflag,output] = fminsearch(@bifurcate,[S1 S2 Stest O2 O7 rho])
end
   
    function err = bifurcate(params)
        thresh='fixed';
        prac=1-normcdf(theta,params(1),params(6));
        study_imm=1-normcdf(theta,params(2),params(6));
        study_2=1-normcdf(theta,params(2)-params(4),params(6));
        study_7=1-normcdf(theta,params(2)-params(5),params(6));
% 
%         pd=makedist('Normal','mu', param(3),'sigma', params(6));
%         trunc=truncate
        test_imm=(1-normcdf(theta,params(3),params(6)))*prac;
        test_2=(1-normcdf(theta,params(3)-params(4),params(6)))*prac;
        test_7=(1-normcdf(theta,params(3)-params(5),params(6)))*prac;
        
        test_imm=(1-normcdf(theta,params(3),params(6)))*prac;
        test_2=(1-normcdf(theta,params(3)-params(4),params(6)))*prac;
        test_7=(1-normcdf(theta,params(3)-params(5),params(6)))*prac;

        pred=[study_imm study_2 study_7 prac test_imm test_2 test_7];
        Lu=(data.*log(data))+((1-data).*log(1-data));
        Lc=(data.*log(pred))+((1-data).*log(1-pred));
        err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
        if (params(1) <=0 || params(2) <= 0 || params(3) <= 0 || params(5) <= params(4)|| ... 
                params(2) < params(1) || params(3) < params(1)  ||   params(3) < params(2))
            err=1000000;
        end
    end

    function err = bifurcate_variability(params)
        thresh='variable';
        count=1000;
        lx=-4;  % smallest value
        mx=4;   % largest value
        rx=(mx-lx)./(count-1);  % step size
        x=lx:rx:mx; % vector of steps

        prac=sum(normpdf(x,params(1),params(6)).*normcdf(x,theta,params(7))).*rx ;
        study_imm=sum(normpdf(x,params(2),params(6)).*normcdf(x,theta,params(7))).*rx ;
        study_2=sum(normpdf(x,params(2)-params(4),params(6)).*normcdf(x,theta,params(7))).*rx ;
        study_7=sum(normpdf(x,params(2)-params(5),params(6)).*normcdf(x,theta,params(7))).*rx ;
        
        test_imm=sum(normpdf(x,params(3),params(6)).*normcdf(x,theta,params(7))).*rx.*prac;
        test_2=sum(normpdf(x,params(3)-params(4),params(6)).*normcdf(x,theta,params(7))).*rx.*prac;
        test_7=sum(normpdf(x,params(3)-params(5),params(6)).*normcdf(x,theta,params(7))).*rx.*prac;
        
        pred=[study_imm study_2 study_7 prac test_imm test_2 test_7];
        Lu=(data.*log(data))+((1-data).*log(1-data));
        Lc=(data.*log(pred))+((1-data).*log(1-pred));
        err=-sum((2*120*(Lc(1:7)-Lu(1:7))));
        if (params(1) <=0 || params(2) <= 0 || params(3) <= 0 || params(5) < params(4) || ...
            params(2) < params(1) || params(3) < params(1)  ||   params(3) < params(2))    
            err=1000000;
        end
    end

        
h=figure(1);
set(h,'Position', [100,50, 1200, 700]);
hold off
    plot([1,2,7], data(1:3), 'b--', [0,1,2,7], data(4:7),'b');
    hold on
    plot([1,2,7], pred(1:3),'r--', [0,1,2,7], pred(4:7),'r');
    legend('Study (obs)','Test (obs)','Study (Bifurcation)', 'Test (Bifurcation)','Location','Northeast');
    text('position',[.25,min([data pred])+.02], ...
         'string', char(['\chi^2{ = }' num2str(chi_square)] , [ 'Threshold: ' thresh]), 'FontWeight','bold');
end