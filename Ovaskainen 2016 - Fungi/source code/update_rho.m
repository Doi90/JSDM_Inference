function rho = update_rho(rho,beta,T,gamma,detQg,iQg,iV,rhopw,ph,outlierspecies)
    [nc ns]=size(beta);
    tmp=[];
    res=beta'-(T*gamma);
    res=res(:);
    for rg = 1:length(detQg)
        iVs = kron(iV,iQg(:,:,rg));
        if outlierspecies
            PHI=(1./sqrt(ph))*(1./sqrt(ph))';
            iVs = kron(iV,(1./PHI).*iQg(:,:,rg));
        else
            iVs = kron(iV,iQg(:,:,rg));
        end
        tmp = [tmp; res'*iVs*res];
    end
    logdetg=-ns*log(det(iV))+nc*detQg;
    like = log(rhopw(:,2))-1/2*logdetg-(1/2)*tmp;
    like = like-max(like);
    like = exp(like);
    like = like/sum(like);
    rho=randsample(length(like),1,true,like);
    