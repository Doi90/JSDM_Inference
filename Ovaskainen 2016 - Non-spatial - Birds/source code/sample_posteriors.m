function [gamma,V,beta,sigma,rho,nf,ph,lambda,alpha,eta,nfs,etas,lambdas] = sample_posteriors(iter,gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep,ny,ns,nc,ncs,nt,nr,phylogeny,spatial,includeXs,alphapw)

ph=[];
beta=reshape(betarep(iter,:),nc,ns);
gamma=reshape(gammarep(iter,:),nt,nc);
V=reshape(Vrep(iter,:),nc,nc);
sigma = diag(sigmarep(iter,:));
rho = [];
if phylogeny
    rho = rhorep(iter);
end
eta = [];
lambda = [];
alpha = [];
nf = [];
if nr>0
    nf = nfrep(iter,:); 
    for i=1:nr
        lambda1=lambdarep{i};
        eta1=etarep{i};         
        nf1=nfrep(:,i);
        count=sum(nf1(1:iter-1));
        lambda{i}=lambda1(:,count+1:count+nf1(iter))';
        eta{i}=eta1(:,count+1:count+nf1(iter));
        alpha{i} = -1;
        if spatial(i)
            alpha1=alpharep{i};
            alpha{i}=alpha1(count+1:count+nf1(iter))';
            for j=1:nf1(iter)
                alpha{i}(j) = find(alphapw{i}(:,1)==alpha{i}(j));
            end
        end
    end
end
nfs=[];
etas=[];
lambdas=[];
if includeXs
    nfs = nfsrep(iter);
    count = sum(nfsrep(1:iter-1));
    lambdas = lambdasrep(count+1:count+nfs,:);
    etas = etasrep(count+1:count+nfs,:)';
end


