function [ psampling, precovery, acc ] =predict(S,R,O,k,weight)
    psampling = 1-((1-(S./(S+O))).^k);
    precovery = R./(R+O);
    acc = sum(psampling.*precovery .* weight);
end

