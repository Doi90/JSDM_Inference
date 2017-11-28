function [] =     save_results(folder,dataset,run,betarep,sigmarep,gammarep,Vrep,rhorep,nfrep,lambdarep,alpharep,etarep,nfsrep,lambdasrep,etasrep,phrep,phylogeny,outlierspecies,includeXs,spatial)
[tmp nr] = size(nfrep);
posteriorfolder=strcat(folder,'posteriors\');
if run==0
    posteriorfolderX = posteriorfolder;
else
    posteriorfolderX=strcat(posteriorfolder,int2str(run),dataset);
    mkdir(posteriorfolderX);
    posteriorfolderX=strcat(posteriorfolderX,'\');
end

filename=strcat(posteriorfolderX,'beta',dataset,'.csv');
csvwrite(filename,betarep);
filename=strcat(posteriorfolderX,'sigma',dataset,'.csv');
csvwrite(filename,sigmarep);
filename=strcat(posteriorfolderX,'gamma',dataset,'.csv');
csvwrite(filename,gammarep);
filename=strcat(posteriorfolderX,'V',dataset,'.csv');
csvwrite(filename,Vrep);
if includeXs
    filename=strcat(posteriorfolderX,'nfs',dataset,'.csv');
    csvwrite(filename,nfsrep);
    filename=strcat(posteriorfolderX,'etas',dataset,'.csv');
    csvwrite(filename,etasrep);
    filename=strcat(posteriorfolderX,'lambdas',dataset,'.csv');
    csvwrite(filename,lambdasrep);
end
if phylogeny
    filename=strcat(posteriorfolderX,'phylo_rho',dataset,'.csv');
    csvwrite(filename,rhorep);
end
if nr>0
    filename=strcat(posteriorfolderX,'nf',dataset,'.csv');
    csvwrite(filename,nfrep);
end
for i1=1:nr
    filename=strcat(posteriorfolderX,'lambda_',int2str(i1),dataset,'.csv');
    csvwrite(filename,lambdarep{i1});
    if spatial(i1)
        filename=strcat(posteriorfolderX,'alpha_',int2str(i1),dataset,'.csv');
        csvwrite(filename,alpharep{i1}');
    end
    filename=strcat(posteriorfolderX,'eta_',int2str(i1),dataset,'.csv');
    csvwrite(filename,etarep{i1});  
end
if outlierspecies
    filename=strcat(posteriorfolderX,'ph',dataset,'.csv');
    csvwrite(filename,phrep);
end