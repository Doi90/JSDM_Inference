function eta = sample_eta_prediction(xy,eta_xy,sce_xy,alpha,alphapw,spatial,sce_np)
nr=length(spatial);
eta = [];
for i1 = 1:nr
    [np,nf]=size(eta_xy{i1});
    sce_npi=sce_np(i1);
    if spatial(i1)
        spatdim = size(xy{i1},2);
        sce_di = zeros(np,sce_npi);
        di = zeros(np,np);
        for j=1:spatdim
            xx=repmat(xy{i1}(:,j),1,sce_npi);
            sce_xx=repmat(sce_xy{i1}(:,j),1,np)';
            dx=xx-sce_xx;
            sce_di=sce_di+dx.^2;
            xx=repmat(xy{i1}(:,j),1,np);
            dx=xx-xx';
            di=di+dx.^2;
        end
        di = sqrt(di);
        sce_di = sqrt(sce_di);
        alpha1=alphapw{i1}(alpha{i1},1);
        eta1 = zeros(sce_npi,nf);
        
        for j = 1:nf
            if (alpha1(j)<1e-5)
                W = eye(np);
                sce_W = eye(np);
            else
                W = exp(-di/alpha1(j));
                sce_W = exp(-sce_di/alpha1(j))';
            end
            iW = inv(W);
            for jj = 1:sce_np
                eta1(jj,j) = sce_W(jj,:)*iW*eta_xy{i1}(:,j);
            end
        end
    else
        eta1 = normrnd(0,1,[sce_npi(i1),nf]);
    end
    eta{i1}=eta1;
end