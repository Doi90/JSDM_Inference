clearvars;
tic

example = 'Mosquito';

[folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial]=model_definitions();
[ny,nc,ncs,ns,nt,nr,np,dist,X,Xs,T,Y,pi,iQg,detQg,rhopw,iWg,detWg,alphapw] = load_data(folder,dataset,phylogeny,spatial,speciesX,includeXs,traits);
[samples,thinning,ny,ns,nc,ncs,nt,nr,outlierspecies,phylogeny,runs,adapt1,adapt2,spatial]=read_metadata(folder,dataset);
include_runs=adapt1:runs;
[gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep] = read_posteriors(folder,dataset,phylogeny,outlierspecies,include_runs,spatial,includeXs);
nmcmc = size(betarep,1);
expected = true; % Whether predictions are made for expected values or according to the data model

iter = unidrnd(nmcmc);
[gamma,V,beta,sigma,rho,nf,ph,lambda,alpha,eta,nfs,etas,lambdas] = sample_posteriors(iter,gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep,ny,ns,nc,ncs,nt,nr,phylogeny,spatial,includeXs,alphapw);
eta_sampled=eta;
for type = 1:3
    if type == 1
        fprintf('Making prediction for training data; estimated latent factors\n');
        eta = eta_sampled;
        Yp = make_prediction(X,Xs,beta,lambda,eta,pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected);
    end
    if type == 2
        fprintf('Making prediction for training data; unknown latent factors\n');
        for i1=1:nr
            eta1 = normrnd(0,1,[np(i1),nf(i1)]);
            eta{i1}=eta1;
        end
        Yp = make_prediction(X,Xs,beta,lambda,eta,pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected);
    end
    if type ==3
        fprintf('Making predictions for species 1-5 in training data; conditional on the other species\n');
        Yc = Y;
        for i = 1:ny
            for j = 1:min(5,ns)
                Yc(i,j) = NaN;
            end
        end
        for i1=1:nr
            eta1 = normrnd(0,1,[np(i1),nf(i1)]);
            eta{i1}=eta1;
        end
        Yp = make_conditional_prediction(10,Yc,X,Xs,beta,lambda,eta,etas,lambdas,pi,dist,sigma,alpha,nf,spatial,iWg,speciesX,includeXs);
    end
    R2 = zeros(1,ns);
    for j=1:ns
        R2(j) = mean(Yp(Y(:,j)==1,j))-mean(Yp(Y(:,j)==0,j));
    end
    fprintf(strcat('Tjur R2 for each species:\n',num2str(R2),'\n'));
    fprintf(strcat('mean Tjur R2 over the species:',num2str(mean(R2)),'\n\n'));
end

if strcmp(example,'Mosquito')
    scenario = strcat(dataset);
    validation = true;
    [sce_X,sce_Y,sce_ny,sce_pi,sce_np,sce_xy,xy] = load_scenario_data(folder,dataset,scenario,nr,spatial,validation);
    for type = 1:2
        if type == 1
            fprintf('Making prediction for the scenario; unknown latent factors\n');
            for i1=1:nr
                eta1 = normrnd(0,1,[sce_np(i1),nf(i1)]);
                eta{i1}=eta1;
            end
        end
        if type ==2
            fprintf('Making prediction for the scenario; spatial latent factors interpolated\n');
            eta = sample_eta_prediction(xy,eta_sampled,sce_xy,alpha,alphapw,spatial,sce_np);
        end
        
        Yp = make_prediction(sce_X,Xs,beta,lambda,eta,sce_pi,sigma,dist,etas,lambdas,speciesX,includeXs,expected);
        R2 = zeros(1,ns);
        for j=1:ns
            R2(j) = mean(Yp(sce_Y(:,j)==1,j))-mean(Yp(sce_Y(:,j)==0,j));
        end
        fprintf(strcat('Tjur R2 for each species:\n',num2str(R2),'\n'));
        fprintf(strcat('mean Tjur R2 over the species:',num2str(mean(R2)),'\n\n'));
    end
end

predictionsfolder=strcat(folder,'predictions\');
filename=strcat(predictionsfolder,'Yp',dataset,'.xlsx');
    xlswrite(filename,Yp)
toc
    
    