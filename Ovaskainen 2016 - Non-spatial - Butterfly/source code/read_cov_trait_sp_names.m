function [covariates,scovariates,traitnames,species]=read_cov_trait_sp_names(folder,dataset,nc,ncs,nt,ns,traits,includeXs)
datafolder=strcat(folder,'data\');

fileID = fopen(strcat(datafolder,'covariates',dataset,'.txt'),'rt');
A = textscan(fileID,'%s');
fclose(fileID);
for i=1:nc
    covariates{i}=A{1}{i};
end

if includeXs
    fileID = fopen(strcat(datafolder,'scovariates',dataset,'.txt'),'rt');
    A = textscan(fileID,'%s');
    fclose(fileID);
    for i=1:ncs
        scovariates{i}=A{1}{i};
    end
else
    scovariates=[];
end

if traits
    fileID = fopen(strcat(datafolder,'traits',dataset,'.txt'),'rt');
    A = textscan(fileID,'%s');
    fclose(fileID);
    for i=1:nt
        traitnames{i}=A{1}{i};
    end
else
    traitnames{1}='';
end

fileID = fopen(strcat(datafolder,'species',dataset,'.txt'),'rt');
A = textscan(fileID,'%s');
fclose(fileID);
for i=1:ns
    species{i}=A{1}{i};
end
