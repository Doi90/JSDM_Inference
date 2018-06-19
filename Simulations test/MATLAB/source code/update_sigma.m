function sigma = update_sigma(X,Xs,beta,eta,lambda,etas,lambdas,pi,dist,asigma,bsigma,z,spatial,speciesX,includeXs)
    if length(spatial)==0
        nr = 0;
    else
        [ny nr]=size(pi);
    end
    [ny ns] = size(z);
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
    eps=z-Ez;
    sigma=eye(ns);
    for j=1:ns
        if not(dist(j)==2)
           sigma(j,j)=1/gamrnd(asigma(j)+ny/2,1./(bsigma(j)+sum(eps(:,j).^2)/2)); 
        end
    end
