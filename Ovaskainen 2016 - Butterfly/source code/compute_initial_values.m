function [beta,gamma,iV,sigma,rho,ph,z,nf,lambda,eta,delta,psijh,alpha,nfs,lambdas,etas,deltas,psijhs]=compute_initial_values(X,Xs,Y,T,pi,dist,phylogeny,V0,mgamma,nur,a1r,b1r,a2r,b2r,nus,a1s,b1s,a2s,b2s,speciesX,spatial);
if speciesX
    [ny nc]=size(X{1});
else
    [ny nc]=size(X);
end
if size(Xs,1)==0
    ncs = 0;
else
    [ny ncs]=size(Xs);
end
[ny ns]=size(Y);
[ns nt]=size(T);
if length(spatial)==0
    np = [];
    nr = 0;
else
    [ny nr]=size(pi);
    np = max(pi);
end
ph = ones([ns 1]);
if nr>0
    %nf = ones(nr,1)*max(2,floor(log(ns)*3));        % initial number of factors. k(1) for residual, k(i1+1) for random effects
    nf = ones(nr,1)*2;        % initial number of factors. k(1) for residual, k(i1+1) for random effects
else
    nf=[];
end
if ncs>0
    nfs = 2;
else
    nfs = 0;
end
sigma = eye(ns);
beta = zeros(nc,ns);
warning('off','all');
with_data=1-isnan(Y);
for j = 1:ns
    if speciesX
        X1=X{j};
    else
        X1=X;
    end
    if sum(with_data(:,j))>nc+1
        if or(dist(j)==1,dist(j)==4)
            beta(:,j) = glmfit(X1(:,2:nc),Y(:,j),'normal');
            res=X1*beta(:,j)-Y(:,j);
            sigma(j,j)=max(0.0001,(res'*res)/ny);
        end
        if dist(j)==2
            beta(:,j) = glmfit(X1(:,2:nc),Y(:,j),'binomial','probit');
        end
        if dist(j)==3
            beta(:,j) = glmfit(X1(:,2:nc),log(Y(:,j)+0.5),'normal');
            res=X1*beta(:,j)-log(Y(:,j)+0.5);
            sigma(j,j)=max(0.0001,(res'*res)/ny);
        end
    end
end
beta=max(min(beta,3),-3);
warning('on','all');
Vn = inv(T'*T +eye(nt));
gamma = Vn*(T'*beta');
be0 = gamma'*T';
V = cov(beta')+0.1*eye(nc);
iV = inv(V);

rho=1;

if nr==0
    alpha=[];
    lambda=[];
    eta=[];
    delta=[];
    psijh=[];
end
for i1=1:nr
    alpha1 = ones(nf(i1),1);
    lambda1 = normrnd(0,1,[nf(i1),ns]);
    eta1 = normrnd(0,1,[np(i1),nf(i1)]);
    alpha{i1}=alpha1;
    lambda{i1}=lambda1;
    eta{i1}=eta1;
end

etas=normrnd(0,1,[ncs,nfs]);
lambdas=normrnd(0,1,[nfs,ns]);

z =  update_z(X,Xs,beta,eta,lambda,etas,lambdas,Y,pi,dist,sigma,speciesX);

for i1=1:nr
    psijh1 = gamrnd(nur/2,2/nur,[ns,nf(i1)]);
    delta1 = [gamrnd(a1r,1/b1r);gamrnd(a2r,1/b2r,[nf(i1)-1,1])];
    psijh{i1} = psijh1;
    delta{i1} = delta1;
end

psijhs = gamrnd(nus/2,2/nus,[ns,nfs]);
deltas = [gamrnd(a1s,1/b1s);gamrnd(a2s,1/b2s,[nfs-1,1])];
