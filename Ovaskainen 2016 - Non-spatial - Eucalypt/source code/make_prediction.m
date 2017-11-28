function Y=make_prediction(X,Xs,beta,lambda,eta,pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected)
[ny nc]=size(X);
[nc ns]=size(beta);
if min(pi)<0
    nr = 0;
else
    [ny nr] = size(pi);
end

if speciesX
    Ez = zeros(ny,ns);
    for i=1:ns
        Ez(:,i)=X{i}*beta(:,i);
    end
else
    Ez = X*beta;
end
if includeXs
    Ez = Ez+Xs*etas*lambdas;
end
for i1=1:nr
    eta1=eta{i1};
    lambda1=lambda{i1};
    Ez = Ez+eta1(pi(:,i1),:)*lambda1;
end
if expected
    for j=1:ns
        if(dist(j)==1)
            Y(:,j)=Ez(:,j);
        end
        if(dist(j)==2)
            Y(:,j)=normcdf(Ez(:,j));
        end
        if(dist(j)==3)
            Y(:,j)=floor(exp(Ez(:,j)));
        end
    end
else
    eps = zeros(ny,ns);
    for j=1:ns
        eps(:,j) = normrnd(0,sqrt(sigma(j,j)),[1,ny]);
    end
    z=Ez+eps;
    for j=1:ns
        if(dist(j)==1)
            Y(:,j)=z(:,j);
        end
        if(dist(j)==2)
            Y(:,j)=z(:,j)>0;
        end
        if(dist(j)==3)
            Y(:,j)=floor(exp(z(:,j)));
        end
    end
end