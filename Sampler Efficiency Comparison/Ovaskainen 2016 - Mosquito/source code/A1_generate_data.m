clearvars;
[folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial]=model_definitions();
datafolder = strcat(folder,'C:\Users\David\Documents\University\PhD\JSDM Comparison\Ovaskainen et al 2015a\mee312502-sup-0004-AppendixS2\mee312502-sup-0003-examples\examples\butterfly\data\');
tvfolder = strcat(datafolder,'true values\');
if (~isequal(exist(tvfolder, 'dir'),7))
    mkdir(tvfolder);
end
rng('shuffle');
example = 'butterfly';
nr = length(spatial);
if strcmp(example,'butterfly')
    ny = 100;    % number of sampling units
    ns = 50;      % number of species
    nc = 3;      % number of habitat predictors, including intercept
    dist=2*ones(ns,1);  % distributional assumptions
    np = 10*ones(1,nr);  % number of levels for each latent factor
    np(1) = ny;
end
if nr==0
    np = [];
end
nf = 2*(1:nr); %number of latent factors for each level
a1r = 5; b1r = 1; a2r = 5; b2r = 1; % prior parameters adjusting the effect of the latent factors
if traits
    nt = 2;      % number of traits, including intercept;
else
    nt = 1;
end
rho = 51; % fraction of variance structured by phylogeny, index from 1 to 101
missing_fraction = -1; % fraction of missing data
if includeXs
    ncs = 20; % number of habitat predictors to be reduced to community level drivers
    nfs = 2; % number of community level drivers
end
sigma=eye(ns);

fileID = fopen(strcat(datafolder,'covariates',dataset,'.txt'),'wt');
for i=1:nc
    fprintf(fileID,'%s\n',strcat('cov_',int2str(i)));
end
fclose(fileID);

if includeXs
    fileID = fopen(strcat(datafolder,'scovariates',dataset,'.txt'),'wt');
    for i=1:ncs
        fprintf(fileID,'%s\n',strcat('scov_',int2str(i)));
    end
    fclose(fileID);
end

if traits
    fileID = fopen(strcat(datafolder,'traits',dataset,'.txt'),'wt');
    for i=1:nt
        fprintf(fileID,'%s\n',strcat('trait_',int2str(i)));
    end
    fclose(fileID);
end

fileID = fopen(strcat(datafolder,'species',dataset,'.txt'),'wt');
for i=1:ns
    fprintf(fileID,'%s\n',strcat('sp_',int2str(i)));
end
fclose(fileID);

if nr>0
    pi=ones([ny nr]);
    for i1 = 1:nr
        for j = 1:ny
            pi(j,i1) = mod(j-1,np(i1))+1;
        end
        if(i1>1)
            pi(:,i1)=pi(randperm(ny),i1);
        end
    end
end

T = normrnd(0,1,[ns,nt]);       % species traits
T(:,1) = 1;
if speciesX
    for i = 1:ns
        X1 = normrnd(0,1,[ny,nc]);
        X1(:,1) = 1;
        X{i}=X1;
    end
else
    X = normrnd(0,1,[ny,nc]);       % habitat covariates
    X(:,1) = 1;
end
gamma = normrnd(0,1,[nt,nc]);
gamma(1) = 0;
df = nc+2;
V = iwishrnd(eye(nc),df)*(df-nc-1);
iV = inv(V);
mbeta = T*gamma;
ph = ones([ns 1]);
if outlierspecies
    for j = 1:min(5,round(ns/10))
        ph(j) = 0.0001;
    end
end

if phylogeny
    C=eye(ns);
    for i1 = 1:2:ns-1
        C(i1,i1+1)=0.99;
        C(i1+1,i1)=0.99;
    end
    rhop = 0:0.01:1;
    rhow = ones(1,length(rhop));
    rhow = 0.5*rhow/(sum(rhow)-1);
    rhow(1) = 0.5;
    Q=rhop(rho)*C+(1-rhop(rho))*eye(ns);
    M = kron(V,Q);
    if outlierspecies
        PHI=(1./sqrt(ph))*(1./sqrt(ph))';
        MM = kron(ones(nc),PHI).*M;
    else
        MM = M;
    end
    beta=mvnrnd(mbeta(:),MM)';
    beta=reshape(beta,ns,nc)';
    csvwrite(strcat(datafolder,'phylo_rho',dataset,'.csv'),[rhop; rhow]');
    csvwrite(strcat(datafolder,'phylo_C',dataset,'.csv'),C);
    csvwrite(strcat(datafolder,'\true values\','phylo_rho',dataset,'.csv'),rhop(rho));
else
    beta = [];
    for j = 1:ns
        beta=[beta; mvnrnd(mbeta(j,:),(1/ph(j))*V)];
    end
    beta=beta';
end

csvwrite(strcat(datafolder,'dist',dataset,'.csv'),dist);
if traits
    csvwrite(strcat(datafolder,'T',dataset,'.csv'),T);
end
if speciesX
    for i=1:ns
        csvwrite(strcat(datafolder,'X',dataset,'_',int2str(i),'.csv'),X{i});
    end
else
    csvwrite(strcat(datafolder,'X',dataset,'.csv'),X);
end
if nr>0
    csvwrite(strcat(datafolder,'LF_units',dataset,'.csv'),pi);
end
csvwrite(strcat(datafolder,'\true values\','gamma',dataset,'.csv'),gamma);
csvwrite(strcat(datafolder,'\true values\','beta',dataset,'.csv'),beta);
csvwrite(strcat(datafolder,'\true values\','V',dataset,'.csv'),V);
csvwrite(strcat(datafolder,'\true values\','ph',dataset,'.csv'),ph);
csvwrite(strcat(datafolder,'\true values\','sigma',dataset,'.csv'),diag(sigma));

if speciesX
    Ez = zeros(ny,ns);
    for i=1:ns
        Ez(:,i)=X{i}*beta(:,i);
    end
else
    Ez = X*beta;
end
spatdim = 2*ones(1,nr);
for i1=1:nr
    alpha1=NaN;
    if spatial(i1)
        xy1=rand(np(i1),spatdim(i1));
        alphap = 0:0.01:1;
        alphaw = ones(1,length(alphap));
        alphaw = 0.5*alphaw/(sum(alphaw)-1);
        alphaw(1) = 0.5;
        csvwrite(strcat(datafolder,'LF_alpha',dataset,'_',int2str(i1),'.csv'),[alphap; alphaw]');
        alpha1 = unidrnd(length(alphap),nf(i1),1);
        di = zeros(np(i1),np(i1));
        for j=1:spatdim(i1)
            xx=repmat(xy1(:,j),1,np(i1));
            dx=xx-xx';
            di=di+dx.^2;
        end
        di=sqrt(di);
        csvwrite(strcat(datafolder,'LF_xy',dataset,'_',int2str(i1),'.csv'),xy1);
        xy{i1} = xy1;
    end
    alpha{i1} = alpha1;
    
    nur = 3;
    
    delta1=zeros(nf(i1),1);
    delta1(1) = gamrnd(a1r,1/b1r);
    for h = 2:nf(i1)
        delta1(h) = gamrnd(a2r,1/b2r);
    end
    tauh1 = cumprod(delta1);
    psijh1 = gamrnd(nur/2,1./(nur/2),nf(i1),ns);
    lambda1=normrnd(0,sqrt(1./(repmat(tauh1,1,ns).*psijh1)));
    
    if spatial(i1)
        eta1=[];
        for h=1:nf(i1)
            if(alpha1(h)>10^(-7))
                W = exp(-di/alphap(alpha1(h)));
            else
                W = eye(length(di));
            end
            eta1 = [eta1; mvnrnd(zeros(np(i1),1),W)];
        end
        eta1 = eta1';
    else
        eta1 = normrnd(0,1,[np(i1),nf(i1)]);
    end
    Ez = Ez + eta1(pi(:,i1),:)*lambda1;
    lambda{i1} = lambda1;
    eta{i1} = eta1;
    delta{i1} = delta1;
    psijh{i1} = psijh1';
end


for i1=1:nr
    filename=strcat(datafolder,'\true values\','lambda_',int2str(i1),dataset,'.csv');
    csvwrite(filename,lambda{i1});
    filename=strcat(datafolder,'\true values\','eta_',int2str(i1),dataset,'.csv');
    csvwrite(filename,eta{i1});
    if spatial(i1)
        filename=strcat(datafolder,'\true values\','alpha_',int2str(i1),dataset,'.csv');
        csvwrite(filename,alphap(alpha{i1})');
    end
end

if includeXs
    etas = normrnd(0,1,[ncs,nfs]);
    lambdas = normrnd(0,1,[nfs,ns]);
    Xs = normrnd(0,1,[ny,ncs]);
    Xel=Xs*etas*lambdas;
    Ez=Ez+Xel;
    
    csvwrite(strcat(datafolder,'\true values\','lambdas',dataset,'.csv'),lambdas);
    csvwrite(strcat(datafolder,'\true values\','etas',dataset,'.csv'),etas);
    csvwrite(strcat(datafolder,'\true values\','nfs',dataset,'.csv'),nfs);
    csvwrite(strcat(datafolder,'Xs',dataset,'.csv'),Xs);
end

z=Ez;
for i=1:ny
    z(i,:)=mvnrnd(Ez(i,:),sigma);
end
Y=z;
for j=1:ns
    if(dist(j)==2)
        Y(:,j)=z(:,j)>0;
    end
end

csvwrite(strcat(datafolder,'y',dataset,'.csv'),Y);
csvwrite(strcat(datafolder,'\true values\','nf',dataset,'.csv'),nf);
