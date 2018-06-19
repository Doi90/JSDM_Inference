function [gammaT,VT,rhoT,betaT,phT,sigmaT,nfT,lambdaT,alphaT,etaT,nfsT,lambdasT,etasT] = read_true_values(folder,dataset,phylogeny,true_values,includeXs,spatial)
datafolder=strcat(folder,'data\');
nr=length(spatial);
gammaT=[];
VT=[];
rhoT=[];
betaT=[];
phT=[];
sigmaT=[];
nfT=[];
if nr==0
    alphaT=[];
end
for i=1:nr
    alphaT{i}=[];
end
etaT=[];
lambdaT=[];
etasT=[];
lambdasT=[];
nfsT=[];
if(true_values)
    filename=strcat(datafolder,'\true values\','gamma',dataset,'.csv');
    gammaT=csvread(filename);
    filename=strcat(datafolder,'\true values\','V',dataset,'.csv');
    VT=csvread(filename);
    if(phylogeny)
        filename=strcat(datafolder,'\true values\','phylo_rho',dataset,'.csv');
        rhoT=csvread(filename);
    end
    filename=strcat(datafolder,'\true values\','beta',dataset,'.csv');
    betaT=csvread(filename);
    filename=strcat(datafolder,'\true values\','ph',dataset,'.csv');
    phT=csvread(filename);
    filename=strcat(datafolder,'\true values\','sigma',dataset,'.csv');
    sigmaT=csvread(filename);
    filename=strcat(datafolder,'\true values\','nf',dataset,'.csv');
    if nr>0
        nfT=csvread(filename);
    end
    for i1=1:nr
        filename=strcat(datafolder,'\true values\','lambda_',int2str(i1),dataset,'.csv');
        lambdaT{i1}=csvread(filename);
        filename=strcat(datafolder,'\true values\','eta_',int2str(i1),dataset,'.csv');
        etaT{i1}=csvread(filename);
        if spatial(i1)
            filename=strcat(datafolder,'\true values\','alpha_',int2str(i1),dataset,'.csv');
            alphaT{i1}=csvread(filename);
        end
    end
    if includeXs
        filename=strcat(datafolder,'\true values\','nfs',dataset,'.csv');
        nfsT=csvread(filename);
        filename=strcat(datafolder,'\true values\','etas',dataset,'.csv');
        etasT=csvread(filename);
        filename=strcat(datafolder,'\true values\','lambdas',dataset,'.csv');
        lambdasT=csvread(filename);
    end
end

