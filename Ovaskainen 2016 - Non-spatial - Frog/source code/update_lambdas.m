function lambdas=update_lambdas(X,Xs,pi,z,lambdas,etas,beta,sigma,eta,lambda,spatial,speciesX)
[ny ncs]=size(Xs);
[nfs ns]=size(lambdas);
if length(spatial)==0
    np = [];
    nr = 0;
else
    [ny nr]=size(pi);
    np = max(pi);
end
S = z;
if speciesX
    for i=1:ns
        S(:,i) = S(:,i) - X{i}*beta(:,i);
    end
else
    S = S - X*beta;
end
for i1=1:nr
    eta1=eta{i1};
    lambda1=lambda{i1};
    S = S-eta1(pi(:,i1),:)*lambda1;
end
tmp=Xs*etas;
for j=1:ns
    Dj=eye(nfs);
    Uj=inv(inv(Dj)+(1/sigma(j,j))*tmp'*tmp);
    mj=Uj*((1/sigma(j,j))*tmp'*S(:,j));
    lambdas(:,j)=mvnrnd(mj,Uj);
end
