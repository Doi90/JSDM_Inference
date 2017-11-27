function etas= update_etas(X,Xs,pi,z,lambdas,beta,sigma,eta,lambda,spatial,speciesX)
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
isigma=inv(sigma);
Uetas=inv(eye(ncs*nfs)+kron(lambdas*isigma*lambdas',Xs'*Xs));
Uetas=(Uetas+Uetas')/2;
tmp=Xs'*S*isigma*lambdas';
metas=Uetas*(tmp(:));
tmp=mvnrnd(metas,Uetas);
etas=reshape(tmp,[ncs,nfs]);
