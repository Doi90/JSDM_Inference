function [] = summarize_etas_lambdas_As(folder,dataset,quantiles,true_values,nfsrep,lambdasrep,etasrep,species,scovariates,lambdasT,etasT)
nfis=unique(nfsrep);
if(length(nfis)==1)
    lambdasT1=lambdasT;
    etasT1=etasT;
    ns=length(species);
    ncs=length(scovariates);
    nmcmc=length(nfsrep);
    resultsfolder=strcat(folder,'results\');
    filenameE=strcat(resultsfolder,'etas',dataset,'.xlsx');
    filenameL=strcat(resultsfolder,'lambdas',dataset,'.xlsx');
    filenameA=strcat(resultsfolder,'As',dataset,'.xlsx');
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
        Am(i,:)=Am1(:);
    end
    for kk=1:5
        mlambdas=mean(lambdas);
        for i=1:nmcmc
            if(corr(mlambdas',lambdas(i,:)')<0)
                lambdas(i,:)=-lambdas(i,:);
                etas(i,:)=-etas(i,:);
            end
        end
    end
    if true_values
        if(corr(mean(lambdas)',lambdasT1(:))<0)
            lambdasT1=-lambdasT1;
            etasT1=-etasT1;
        end
        AmT1=etasT1*lambdasT1;
    else
        AmT1=[];
    end
    
    for k=0:(length(quantiles)+true_values)
        if k==0
            valsE=reshape(mean(etas),nfis,ncs);
            valsL=reshape(mean(lambdas),nfis,ns);
            valsA=reshape(mean(Am),ncs,ns);
        elseif k<=length(quantiles)
            valsE=reshape(quantile(etas,quantiles(k)),nfis,ncs);
            valsL=reshape(quantile(lambdas,quantiles(k)),nfis,ns);
            valsA=reshape(quantile(Am,quantiles(k)),ncs,ns);
        else
            valsE=etasT1;
            valsL=lambdasT1;
            valsA=AmT1;
        end
        for var=1:3
            if var==1
                nrows=ncs;
                ncols=nfis;
            end
            if var==2
                nrows=ns;
                ncols=nfis;
            end
            if var==3
                nrows=ns;
                ncols=ncs;
            end
            A = cell(nrows+1,ncols+1);
            A{1,1}='';
            for i=1:nrows
                if var==1
                    A{i+1,1}=scovariates{i};
                else
                    A{i+1,1}=species{i};
                end
            end
            for j=1:ncols
                if var==3
                    A{1,j+1}=scovariates{j};
                else
                    A{1,j+1}=strcat('driver ',int2str(j));
                end
            end
            for i=1:nrows
                for j=1:ncols
                    if var==1
                        A{i+1,j+1}=valsE(j,i);
                    end
                    if var==2
                        A{i+1,j+1}=valsL(j,i);
                    end
                    if var==3
                        A{i+1,j+1}=valsA(j,i);
                    end
                end
            end
            if var==1
                filename=filenameE;
            end
            if var==2
                filename=filenameL;
            end
            if var==3
                filename=filenameA;
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
end
warning('on','all');
