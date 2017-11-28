function eta = update_eta(z,X,Xs,beta,sigma,eta,alpha,lambda,etas,lambdas,nf,pi,spatial,iWg,speciesX,includeXs)
    [ny ns] = size(z);
    isigma = inv(sigma);
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
        lambda1=lambda{i1};
        if spatial(i1)==0 % NON SPATIAL LATENT FACTORS
            if sum(lpi==(1:ny)')==ny % i1 corresponds to residuals, use a faster updater
                Veta1 = eye(nf(i1)) + lambda1*isigma*lambda1';
                Tx = cholcov(Veta1);
                [Q,R] = qr(Tx);
                iR = inv(R);
                Veta = iR*iR';                     
                Meta = S*isigma*lambda1'*Veta;                        % ny x k 
                eta1 = Meta + normrnd(0,1,[ny,nf(i1)])*iR';       % update eta1 in a block
            else % i1 corresponds to a random effect, use a more general updater
                eta1=eta{i1};
                for p = 1:np(i1)
                    rows = lpi==p;
                    Veta1 = eye(nf(i1)) + (lambda1*isigma*lambda1')*sum(rows);
                    Tx = cholcov(Veta1); [Q,R] = qr(Tx);
                    iR = inv(R); Veta = iR*iR';                      % Veta = inv(Veta1)
                    Meta = ones(1,sum(rows))*S(lpi==p,:)*isigma*lambda1'*Veta;    % ny x k 
                    eta1(p,:) = Meta + normrnd(0,1,[1,nf(i1)])*iR';
                end
            end
        else % SPATIAL LATENT FACTORS
            iWg1 = iWg{i1};
            alpha1=alpha{i1};
            if sum(lpi==(1:ny)')==ny % i1 corresponds to spatial residuals, use a faster updater
                %fprintf('using spatial updater\n');
                iWs = iWg1(:,:,alpha1(1));
                for h=2:nf(i1)
                    iWs = blkdiag(iWs,iWg1(:,:,alpha1(h)));
                end
                tmp1 = lambda1*isigma;
                tmp1s = kron(tmp1,eye(ny));
                tmp2 = tmp1*lambda1';
                tmp2s = kron(tmp2,eye(ny));
                iUeta = iWs + tmp2s;
                Ueta = inv(iUeta);
                Ueta = (Ueta+Ueta')/2;
                fS = S(:);
                Meta = Ueta*(tmp1s*fS);
                feta = mvnrnd(Meta,Ueta);       % update eta1 in a block
                eta1 = reshape(feta,[ny,nf(i1)]);
            else % i1 corresponds to a spatial random effect, use a more general updater
                fprintf('not implemented yet');
            end
        end
        eta{i1} = eta1;
    end
