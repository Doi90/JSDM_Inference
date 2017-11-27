function Yp = make_conditional_prediction(nmcmc,Yc,X,Xs,beta,lambda,eta,etas,lambdas,pi,dist,sigma,alpha,nf,spatial,iWg,speciesX,includeXs)
[ny nc]=size(X);
[nc ns]=size(beta);
if min(pi)<0
    nr = 0;
else
    [ny nr] = size(pi);
    np = max(pi);
end

for i=1:nmcmc
    z = update_z(X,Xs,beta,eta,lambda,etas,lambdas,Yc,pi,dist,sigma,speciesX);
    if (i<nmcmc)
        eta = update_eta(z,X,Xs,beta,sigma,eta,alpha,lambda,etas,lambdas,nf,pi,spatial,iWg,speciesX,includeXs);
    end
end
Yp=Yc;
for j=1:ns
    if(dist(j)==1)
        Yp(:,j)=z(:,j);
    end
    if(dist(j)==2)
        Yp(:,j)=z(:,j)>0;
    end
    if(dist(j)==3)
        Yp(:,j)=floor(exp(z(:,j)));
    end
end
