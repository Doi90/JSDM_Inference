function [] = summarize_beta(folder,dataset,quantiles,true_values,betarep,species,covariates,betaT)
ns=length(species);
nc=length(covariates);
resultsfolder=strcat(folder,'results\');
filename=strcat(resultsfolder,'beta',dataset,'.xlsx');
warning('off','all');
for k=0:(length(quantiles)+true_values)
    if k==0
        vals=reshape(mean(betarep),nc,ns);
    elseif k<=length(quantiles)
        vals=reshape(quantile(betarep,quantiles(k)),nc,ns);
    else
        vals=betaT;
    end
    A = cell(ns+1,nc+1);
    A{1,1}='';
    for i=1:ns
        A{i+1,1}=species{i};
    end
    for j=1:nc
        A{1,j+1}=covariates{j};
    end
    for i=1:ns
        for j=1:nc
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
warning('on','all');
