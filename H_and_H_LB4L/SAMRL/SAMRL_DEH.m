function [  err, data ] = SAMRL_DEH(params,data,fix_params,free_params)

    utils = getUtils;
    [S_params, R_params, O_params, k, p, ~, nSubs] = utils.findNamedPars(params, fix_params, free_params);
    
    if ( any(S_params <= 0)|| any(R_params <= 0) || any(diff(R_params) < 0) || any(diff(S_params) < 0) || any(diff(O_params) < 0) || (p > 1 || p <=0))
        err=1000000;
    else    
        O1 = O_params(2);
        O2 = O_params(3);
        S1 = S_params(1);
        S2 = S_params(2);
        R1 = R_params(1);
        R2 = R_params(2);
        figure(1);

        % test practice
        T=samp(S1,1)*recov(R1,1);
        subplot(2,1,1);
        hold off
        plot(.8,T,'+k');
        hold on
        plot(.8,.55,'og');
        plot(.8,.57,'og');
        plot(.8,.52,'og');
        plot(.8,.52,'og');

        % baseline
        base(1)=samp(S1,O1)*recov(R1,O1);
        base(2)=samp(S1,O2)*recov(R1,O2);
        plot(base,'-k');
        plot([.44 .28],'ok');

        % restudy
        restudy(1)=samp(S2,O1) * recov(R1,O1);
        restudy(2)=samp(S2,O2) * recov(R1,O2);
        plot(restudy,'-b');
        plot([.81 .53],'ob');

        % same cue test practice
        same_test_cor(1)=samp(S2,O1)*recov(R2,O1); % conditioned on correct test practice
        same_test_inc(1)=((1-samp(S1,1))/(1-T))*samp(S1,O1)*recov(R1,O1); % conditioned on incorect test practice. Calculate proportion of incorrect items that were never sampled
        same_test(1)=T*same_test_cor(1)+(1-T)*same_test_inc(1);

        same_test_cor(2)=samp(S2,O2)*recov(R2,O2); % conditioned on correct test practice
        same_test_inc(2)=samp(S1,O2)*recov(R1,O2); % conditioned on incorrect test practice
        same_test(2)=T*same_test_cor(2)+(1-T)*same_test_inc(2);
        plot(same_test,'-r');
        plot([.56 .46],'or');


        % other cue test practice
        other_test_cor(1)=samp(S1,O1)*recov(R2,O1); % conditioned on correct test practice
        other_test_inc(1)=samp(S1,O1)*recov(R1,O1); % conditioned on incorrect test practice
        other_test(1)=T*other_test_cor(1)+(1-T)*other_test_inc(1);

        other_test_cor(2)=samp(S1,O2)*recov(R2,O2); % conditioned on correct test practice
        other_test_inc(2)=samp(S1,O2)*recov(R1,O2); % conditioned on incorrect test practice
        other_test(2)=T*other_test_cor(2)+(1-T)*other_test_inc(2);
        plot(other_test,'--r');
        plot([.55 .32],'or');

        axis([.5 2.25 0 1]);

        % plot conditionals for same cue and other cue conditions

        subplot(2,1,2);
        hold off
        plot(same_test_cor,'-r');
        hold on
        plot(other_test_cor,'--r');
        plot(same_test_inc,'-k');
        plot(other_test_inc,'--k');

        axis([.5 2.25 0 1]);

        obs = [[.55 .57 .52 .52] [.44 .28]  [.81 .53] [.56 .46] [.55 .32] [.94 .83]  [.04 .05]  [.64 .36] [.45 .22] ];
        pred = [ T T T T base  restudy same_test other_test  same_test_cor same_test_inc other_test_cor  other_test_inc];           

        Lu=(obs.*log(obs))+((1-obs).*log(1-obs));
        Lc=(obs.*log(pred))+((1-obs).*log(1-pred));
        err=-sum(2*nSubs*(Lc(1:(end-4))-Lu(1:(end-4))));    
        
    end
    
    function prob=samp(S,O)
        prob=(1-(1-(S/(S+O)))^k);
    end

    function prob=recov(R,O)
        prob=R/(R+O);
    end


end

