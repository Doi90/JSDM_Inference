function [ny,nc,ncs,ns,nt,nr,np,dist,X,Xs,T,Y,pi,iQg,detQg,rhopw,iWgA,detWgA,alphapwA] = load_data(folder,dataset,phylogeny,spatial,speciesX,includeXs,traits)
    datafolder=strcat(folder,'data\');
    fprintf('Reading data.\n');

    dist = csvread(strcat(datafolder,'dist',dataset,'.csv'));
    Y = csvread(strcat(datafolder,'y',dataset,'.csv'));
    [ny ns]=size(Y);
    
    if speciesX
        for i=1:ns
            X1 = csvread(strcat(datafolder,'X',dataset,'_',int2str(i),'.csv'));
            X{i}=X1;
        end
    else
        X = csvread(strcat(datafolder,'X',dataset,'.csv'));
    end    
    if includeXs
        Xs = csvread(strcat(datafolder,'Xs',dataset,'.csv'));
       [ny ncs]=size(Xs);
    else
        Xs = [];
        ncs = 0;
    end
    if traits
        T = csvread(strcat(datafolder,'T',dataset,'.csv'));
    else
        T = ones(ns,1);
    end
    if speciesX
        [ny nc]=size(X{1});
    else
        [ny nc]=size(X);
    end
    [ns nt]=size(T);

    iQg=[];
    detQg=[];
    rhopw=[];
    if phylogeny
        rhopw=csvread(strcat(datafolder,'phylo_rho',dataset,'.csv'));
        C=csvread(strcat(datafolder,'phylo_C',dataset,'.csv'));      
        for rg=1:length(rhopw)
            rho = rhopw(rg,1);
            Q=rho*C+(1-rho)*eye(ns);         
            iQg = cat(3,iQg,inv(Q));
            cQ = chol(Q);
            detQg = [detQg; 2*sum(log(diag(cQ)))];
        end
    end
    
    nr = length(spatial);
    if nr==0
        np = [];
        pi = [];
    end
    if nr>0
        pi = csvread(strcat(datafolder,'LF_units',dataset,'.csv'));
        [ny nr]=size(pi);
        pio=pi;
        for i=1:nr
            uni=unique(pio(:,i),'stable');
            for j=1:length(uni)
                inds=find(pio(:,i)==uni(j));
                pi(inds,i)=repmat(j,1,length(inds));
            end
        end
        np = max(pi);
    end
    if nr==0
        iWgA=[];
        detWgA=[];
        alphapwA=[];
    end
    spatdim = zeros(nr,1);
    for i1=1:nr
        iWg=[];
        detWg=[];
        alphapw=[];
        if spatial(i1)
            xy=csvread(strcat(datafolder,'LF_xy',dataset,'_',int2str(i1),'.csv'));
            spatdim(i1)=size(xy,2);
            di = zeros(np(i1),np(i1));
            for j=1:spatdim(i1)
                xx=repmat(xy(:,j),1,np(i1));
                dx=xx-xx';
                di=di+dx.^2;
            end
            distance=sqrt(di);
            alphapw=csvread(strcat(datafolder,'LF_alpha',dataset,'_',int2str(i1),'.csv'));
            for ag=1:length(alphapw)
                alpha = alphapw(ag,1);
                if (alpha<1e-5)
                    W = eye(length(xy));
                else
                    W = exp(-distance/alpha);
                end
                iWg = cat(3,iWg,inv(W));
                cholW = chol(W);
                detWg = [detWg; 2*sum(log(diag(cholW)))];
            end
        end
        iWgA{i1}=iWg;
        detWgA{i1}=detWg;
        alphapwA{i1}=alphapw;
    end       

    fprintf(strcat('Sampling units:',int2str(ny),', species:',int2str(ns),', covariates:',int2str(nc),', covariates for dimension reduction:',int2str(ncs),', traits:',int2str(nt),'\n'));
    fprintf(strcat('Response variables: Normal: ',int2str(sum(dist==1)),', Bernoulli: ',int2str(sum(dist==2)),', Rounded log-normal: ',int2str(sum(dist==3)),', Rounded and truncated normal: ',int2str(sum(dist==4)),'\n')); 
    fprintf(strcat('Species specific environmental covariance matrices: '));
    if speciesX
        fprintf(strcat('yes \n'));
    else
        fprintf(strcat('no \n'));
    end
    fprintf(strcat('Environmental covariance matrices for which dimension reduction will be applied: '));
    if speciesX
        fprintf(strcat('yes \n'));
    else
        fprintf(strcat('no \n'));
    end    
    fprintf(strcat('Phylogeny included: '));
    if phylogeny
        fprintf(strcat('yes \n'));
    else
        fprintf(strcat('no \n'));
    end
    for (i1=1:nr)
        fprintf('Latent factor ');
        fprintf(int2str(i1));
        if (spatial(i1))
            fprintf(strcat(': spatial, dimension: ',int2str(spatdim(i1)),', '));
        else
            fprintf(': non-spatial, ');   
        end
        fprintf(strcat(int2str(np(i1)),' levels \n'));
    end
    