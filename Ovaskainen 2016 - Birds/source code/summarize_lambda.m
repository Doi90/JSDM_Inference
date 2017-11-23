function [] = summarize_lambda(folder,dataset,quantiles,true_values,lambdarep,nfrep,sigmarep,species,lambdaT,sigmaT,nr)
ns=length(species);
resultsfolder=strcat(folder,'results\');
warning('off','all');
for ii=1:nr
    lambda1=lambdarep{ii};
    nf1=nfrep(:,ii);
    omegarep=[];
    Rrep=[];
    iRrep=[];
    count=0;
    for j=1:length(nf1)
        lambda2=lambda1(:,count+1:count+nf1(j));
        omega2=lambda2*lambda2';
        R2=corrcov(omega2+diag(sigmarep(j,:)));
        R2=(R2+R2')/2;
        tmp=inv(R2);
        tmp=(tmp+tmp')/2;
        iR2=-corrcov(tmp);
        omegarep=[omegarep, omega2(:)];
        Rrep=[Rrep, R2(:)];
        iRrep=[iRrep, iR2(:)];
        count=count+nf1(j);
    end
    omegarep = omegarep';
    Rrep = Rrep';
    iRrep = iRrep';
    omegaT = [];
    RT = [];
    iRT = [];
    if true_values
        omegaT = lambdaT{ii}'*lambdaT{ii};
        RT = corrcov(omegaT+diag(sigmaT));
        RT = (RT+RT')/2;
        iRT = -corrcov(inv(RT));
    end
    for type = 1:3
        if type == 1
            crep = omegarep;
            cT = omegaT;
            filename=strcat(resultsfolder,'Omega',int2str(ii),dataset,'.xlsx');
        end
        if type == 2
            crep = Rrep;
            cT = RT;
            filename=strcat(resultsfolder,'R',int2str(ii),dataset,'.xlsx');
        end
        if type == 3
            crep = iRrep;
            cT = iRT;
            filename=strcat(resultsfolder,'iR',int2str(ii),dataset,'.xlsx');
        end
        for k=0:(length(quantiles)+true_values)
            if k==0
                vals=reshape(mean(crep),ns,ns);
            elseif k<=length(quantiles)
                vals=reshape(quantile(crep,quantiles(k)),ns,ns);
            else
                vals=cT;
            end
            A = cell(ns+1,ns+1);
            A{1,1}='';
            for i=1:ns
                A{i+1,1}=species{i};
            end
            for j=1:ns
                A{1,j+1}=species{j};
            end
            for i=1:ns
                for j=1:ns
                    A{i+1,j+1}=vals(j,i);
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
end
warning('on','all');