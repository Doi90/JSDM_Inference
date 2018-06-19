tic

[folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial]=model_definitions();
[ny,nc,ncs,ns,nt,nr,np,dist,X,Xs,T,Y,pi,iQg,detQg,rhopw,iWg,detWg,alphapw] = load_data(folder,dataset,phylogeny,spatial,speciesX,includeXs,traits);
[samples,thinning,ny,ns,nc,ncs,nt,nr,outlierspecies,phylogeny,runs,adapt1,adapt2,spatial]=read_metadata(folder,dataset);
include_runs=adapt1:runs;
[gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep] = read_posteriors(folder,dataset,phylogeny,outlierspecies,include_runs,spatial,includeXs);
nmcmc = size(betarep,1);
iter = unidrnd(nmcmc);
[gamma,V,beta,sigma,rho,nf,ph,lambda,alpha,eta,nfs,etas,lambdas] = sample_posteriors(iter,gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep,ny,ns,nc,ncs,nt,nr,phylogeny,spatial,includeXs,alphapw);
expected = true;

scenario = strcat(dataset);
validation = true;
[sce_X,sce_Y,sce_ny,sce_pi,sce_np,sce_xy,xy] = load_scenario_data(folder,dataset,scenario,nr,spatial,validation);

iters = 100;
SYp=zeros(ny,ns);
sce_SYp=zeros(sce_ny,ns);
if nr==0
    varabs=zeros(nc-1,ns);
else nr>0
    varpart=zeros(nc+nf-1,ns);
    Seta = zeros(np(1),nf);
    sce_Seta = zeros(sce_np(1),nf);
end
for ii = 1:iters
    iter = unidrnd(nmcmc);
    [gamma,V,beta,sigma,rho,nf,ph,lambda,alpha,eta,nfs,etas,lambdas] = sample_posteriors(iter,gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep,ny,ns,nc,ncs,nt,nr,phylogeny,spatial,includeXs,alphapw);
    if nr==0
        tmp=zeros(nc-1,ns);
    else
        tmp=zeros(nc+nf-1,ns);
    end
    for j=2:nc
        tmp(j-1,:)=var(X(:,j))*(beta(j,:).^2);
    end
    if nr>0
        
        for j=1:nf
            tmp(nc+j-1,:)=lambda{1}(j,:).^2;
        end
        varpart=varpart+tmp./repmat(sum(tmp),nc+nf-1,1);
    end
    %varabs=varabs+tmp;
    
    eta_sampled=eta;
    Yp = make_prediction(X,Xs,beta,lambda,eta_sampled,pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected);
    eta = sample_eta_prediction(xy,eta_sampled,sce_xy,alpha,alphapw,spatial,sce_np);
    sce_Yp = make_prediction(sce_X,Xs,beta,lambda,eta,sce_pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected);
    sce_SYp = sce_SYp + sce_Yp;
    SYp = SYp + Yp;
    if nr>0
        Seta = Seta + eta_sampled{1};
        sce_Seta = sce_Seta + eta{1};
    end
end
sce_Yp = sce_SYp./iters;
Yp = SYp./iters;
if nr>0
    eta = Seta./iters;
    sce_eta = sce_Seta./iters;
    varpart = varpart./iters;
end
%varabs = varabs./iters;
R2 = zeros(1,ns);
for j=1:ns
    R2(j) = mean(sce_Yp(sce_Y(:,j)==1,j))-mean(sce_Yp(sce_Y(:,j)==0,j));
end
fprintf(strcat('Tjur R2 for each species:\n',num2str(R2),'\n'));
fprintf(strcat('mean Tjur R2 over the species:',num2str(mean(R2)),'\n\n'));
predictionsfolder=strcat(folder,'predictions\');
filename=strcat(predictionsfolder,'Y',dataset,'.xlsx');
xlswrite(filename,Yp);
filename=strcat(predictionsfolder,'Y',scenario,'.xlsx');
xlswrite(filename,sce_Yp);
if nr>0
    filename=strcat(predictionsfolder,'eta',dataset,'.xlsx');
    xlswrite(filename,eta);
    filename=strcat(predictionsfolder,'eta',scenario,'.xlsx');
    xlswrite(filename,sce_eta);
    filename=strcat(predictionsfolder,'varpart',dataset,'.xlsx');
    xlswrite(filename,varpart);
end

fprintf('mean variance components:\n');
%mean(varabs')

toc