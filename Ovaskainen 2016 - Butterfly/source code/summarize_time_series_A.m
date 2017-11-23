function [] = summarize_time_series_A(folder,dataset,quantiles,true_values,nfsrep,lambdasrep,etasrep,betarep,species,covariates,scovariates,lambdasT,etasT,betaT)
nc=length(covariates);
nfis=unique(nfsrep);
if(length(nfis)==1)
    lambdasT1=lambdasT;
    etasT1=etasT;
    ns=length(species);
    ncs=length(scovariates);
    nmcmc=length(nfsrep);
    resultsfolder=strcat(folder,'results\');
    filename=strcat(resultsfolder,'time_series_A',dataset,'.xlsx');
    warning('off','all');
    
    lambdas = zeros(nmcmc,nfis*ns);
    etas = zeros(nmcmc,nfis*ncs);
    Am=zeros(nmcmc,ns*ncs);
    lambdasA=lambdasrep;
    etasA=etasrep;
    for i=1:nmcmc
        lambdas1=lambdasA(1:nfis,:);
        lambdas(i,:)=lambdas1(:);
        lambdasA=lambdasA(nfis+1:size(lambdasA,1),:);
        etas1=etasA(1:nfis,:);
        etas(i,:)=etas1(:);
        etasA=etasA(nfis+1:size(etasA,1),:);
        Am1=etas1'*lambdas1;
        beta=reshape(betarep(i,:),nc,ns);
        beta = beta(2,:);
        for j=1:ns
            Am1(j,j)=Am1(j,j)+beta(j);
        end
        Am(i,:)=Am1(:);
    end
    if true_values
        AmT1=etasT1*lambdasT1;
        for j=1:ns
            AmT1(j,j)= AmT1(j,j)+betaT(2,j);
        end
    else
        AmT1=[];
    end
    
    for k=0:(length(quantiles)+true_values)
        if k==0
            valsA=reshape(mean(Am),ncs,ns);
        elseif k<=length(quantiles)
            valsA=reshape(quantile(Am,quantiles(k)),ncs,ns);
        else
            valsA=AmT1;
        end
        nrows=ns;
        ncols=ncs;
        A = cell(nrows+1,ncols+1);
        A{1,1}='';
        for i=1:nrows
            A{i+1,1}=species{i};
            
        end
        for j=1:ncols
            A{1,j+1}=scovariates{j};
        end
        for i=1:nrows
            for j=1:ncols
                
                A{i+1,j+1}=valsA(j,i);
            end
        end
        
        if k==0
            xlswrite(filename,A,1);
        elseif k<=length(quantiles)
            xlswrite(filename,A,num2str(quantiles(k)));
        else
            xlswrite(filename,A,'true values');
        end
    end
end
warning('on','all');
