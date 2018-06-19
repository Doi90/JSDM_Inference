function [betarep,gammarep,sigmarep,rhorep,Vrep,lambdarep,etarep,alpharep,nfrep,phrep,lambdasrep,etasrep,nfsrep] = add_to_output(mcmc,thinning,beta,gamma,sigma,rhopw,rho,iV,lambda,eta,alpha,ph,nf,lambdas,etas,nfs,betarep,gammarep,sigmarep,rhorep,Vrep,lambdarep,etarep,alpharep,nfrep,phrep,lambdasrep,etasrep,nfsrep,alphapw,phylogeny,spatial)

if mod(mcmc,thinning)==0
    fprintf(strcat('iteration round:',int2str(mcmc),'\n'));
    ii = mcmc/thinning;
    betarep(ii,:) = beta(:);
    gammarep(ii,:) = gamma(:);
    sigmarep(ii,:) = diag(sigma);
    if phylogeny
        rhorep(ii,:)=rhopw(rho,1);
    end
    V = inv(iV);
    Vrep(ii,:) = V(:);
    phrep(ii,:)=ph;
    nfsrep(ii,:)=nfs;
    lambdasrep=[lambdasrep lambdas'];
    etasrep=[etasrep etas];
    nr = length(nf);
    for i1=1:nr
        lambdarep{i1} = [lambdarep{i1} lambda{i1}'];
        etarep{i1} = [etarep{i1} eta{i1}];
        alpha1 = alpha{i1};
        if spatial(i1)
            alphapw1 = alphapw{i1};
            tmp = alphapw1(alpha1,1);
        else
            tmp = -1*ones(length(alpha1),1);
        end
        alpharep{i1} = [alpharep{i1}; tmp];
    end
    if nr>0
        nfrep(ii,:) = nf;
    end
end

