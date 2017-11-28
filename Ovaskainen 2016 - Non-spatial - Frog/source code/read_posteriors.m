function [gamma,V,beta,sigma,rho,nf,ph,lambda,alpha,eta,nfs,etas,lambdas] = read_posteriors(folder,dataset,phylogeny,outlierspecies,include_runs,spatial,includeXs)
posteriorfolder=strcat(folder,'posteriors\');
gamma=[];
V=[];
beta=[];
sigma=[];
rho=[];
nf=[];
ph=[];
lambda=[];
alpha=[];
eta=[];
nfs=[];
etas=[];
lambdas=[];
nr=length(spatial);
for i1=1:nr
    lambda{i1}=[];
    alpha{i1}=[];
    eta{i1}=[];
end
for i=1:length(include_runs)
    run=include_runs(i);
    posteriorfolderX=strcat(posteriorfolder,int2str(run),dataset,'\');
    filename=strcat(posteriorfolderX,'gamma',dataset,'.csv');
    gamma=[gamma; csvread(filename)];
    filename=strcat(posteriorfolderX,'V',dataset,'.csv');
    V=[V; csvread(filename)];
    filename=strcat(posteriorfolderX,'beta',dataset,'.csv');
    beta=[beta; csvread(filename)];
    filename=strcat(posteriorfolderX,'sigma',dataset,'.csv');
    sigma=[sigma; csvread(filename)];
    if includeXs
        filename=strcat(posteriorfolderX,'nfs',dataset,'.csv');
        nfs=[nfs; csvread(filename)];
        filename=strcat(posteriorfolderX,'etas',dataset,'.csv');
        etas=[etas; csvread(filename)'];
        filename=strcat(posteriorfolderX,'lambdas',dataset,'.csv');
        lambdas=[lambdas; csvread(filename)'];
    end
    if phylogeny
        filename=strcat(posteriorfolderX,'phylo_rho',dataset,'.csv');
        rho=[rho; csvread(filename)];
    end
    if nr>0
        filename=strcat(posteriorfolderX,'nf',dataset,'.csv');
        nf=[nf; csvread(filename)];
    end
    for i1=1:nr
        filename=strcat(posteriorfolderX,'lambda_',int2str(i1),dataset,'.csv');
        lambda{i1}=[lambda{i1}; csvread(filename)'];
        if spatial(i1)
            filename=strcat(posteriorfolderX,'alpha_',int2str(i1),dataset,'.csv');
            alpha{i1}=[alpha{i1}; csvread(filename)'];
        end
        filename=strcat(posteriorfolderX,'eta_',int2str(i1),dataset,'.csv');
        eta{i1}=[eta{i1}; csvread(filename)'];
    end
    if outlierspecies
        filename=strcat(posteriorfolderX,'ph',dataset,'.csv');
        ph=[ph; csvread(filename)];
    end
end
for i1=1:nr
    lambda{i1}=lambda{i1}';
    eta{i1}=eta{i1}';
end;
etas=etas;
lambdas=lambdas;





