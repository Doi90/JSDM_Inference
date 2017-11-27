function [betarep,gammarep,Vrep,rhorep,sigmarep,nfrep,lambdarep,phrep,etarep,alpharep,nfsrep,lambdasrep,etasrep] = initialize_output(samples,nc,ncs,ns,nt,nr);
betarep = zeros(samples,nc*ns);                       % regression coefficient values
gammarep = zeros(samples,nt*nc);
Vrep = zeros(samples,nc*nc);                        % covariance in reg coefficients across species
rhorep = zeros(samples,1);                             % evolution of factors across replicates
sigmarep = zeros(samples,ns);                             % evolution of factors across replicates
nfrep = zeros(samples,nr);                         % evolution of factors across replicates
phrep=zeros(samples,ns);
nfsrep=zeros(samples,1);
if nr==0
    lambdarep=[];
    etarep=[];
    alpharep=[];
end
for i1 = 1:nr
    lambdarep{i1} = [];
    etarep{i1} = [];
    alpharep{i1} = [];
end
lambdasrep=[];
etasrep=[];

