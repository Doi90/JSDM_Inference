function [thinned,saveint,ny,ns,nc,ncs,nt,nr,outlierspecies,phylogeny,runs,adapt1,adapt2,spatial]=read_metadata(folder,dataset)
resultfolder=strcat(folder,'posteriors\');
meta=csvread(strcat(resultfolder,'metadata',dataset,'.csv'));
label={'samples','thinning','ny','ns','nc','ncs','nt','nr','outlierspecies','phylogeny','runs','adapt1','adapt2'};
thinned=meta(1);
saveint=meta(2);
ny=meta(3);
ns=meta(4);
nc=meta(5);
ncs=meta(6);
nt=meta(7);
nr=meta(8);
outlierspecies=meta(9);
phylogeny=meta(10);
runs=meta(11);
adapt1=meta(12);
adapt2=meta(13);
if nr==0
    spatial = []
else
    spatial=zeros(1,nr);
end
for i=1:length(label)
    fprintf(strcat(label{i},':',int2str(meta(i)),'\n'));
end
for i=1:nr
    spatial(i)=meta(length(label)+i);
    fprintf(strcat('spatial_',int2str(i),':',int2str(spatial(i)),'\n'));
end
