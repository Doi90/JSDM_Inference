function lambda =  update_lambda(X,Xs,z,beta,sigma,eta,lambda,etas,lambdas,delta,psijh,pi,nf,spatial,speciesX,includeXs)
    [ny ns] = size(z);    
    if length(spatial)==0
        np = [];
        nr = 0;
    else
        [ny nr]=size(pi);
        np = max(pi);
    end    
    for i1=1:nr
        S = z;
        if speciesX
            for i=1:ns
                S(:,i) = S(:,i) - X{i}*beta(:,i);
            end                
        else
            S = S - X*beta;
        end
        if includeXs
            S = S - Xs*etas*lambdas;
        end
        for i2=1:nr
            if ~(i1==i2)  % subtract factor effects of other levels
                eta1=eta{i2};
                lambda1=lambda{i2};
                S = S-eta1(pi(:,i2),:)*lambda1;
            end
        end
        lpi = pi(:,i1);
        eta1=eta{i1};
        eta1=eta1(lpi,:);
        lambda1=lambda{i1};
        delta1=delta{i1};
        psijh1=psijh{i1};
        tauh = cumprod(delta1);  
        Plam = bsxfun(@times,psijh1,tauh');  
        eta2 = eta1'*eta1;
        for j = 1:ns
            Qlam = diag(Plam(j,:)) + (1/sigma(j,j))*eta2;
            blam = (1/sigma(j,j))*eta1'*S(:,j);
            Llam = chol(Qlam,'lower');
            zlam = normrnd(0,1,nf(i1),1);
            vlam = Llam\blam; mlam = Llam'\vlam; ylam = Llam'\zlam;
            lambda1(:,j) = (ylam + mlam);
        end        
        lambda{i1}=lambda1;
    end
    