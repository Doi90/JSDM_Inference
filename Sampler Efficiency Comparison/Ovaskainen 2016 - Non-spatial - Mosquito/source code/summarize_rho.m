function [] = summarize_rho(folder,dataset,quantiles,true_values,rhorep,rhoT)
resultsfolder=strcat(folder,'results\');
filename=strcat(resultsfolder,'rho',dataset,'.xlsx');
warning('off','all');
for k=0:(length(quantiles)+true_values)
    if k==0
        vals=mean(rhorep);
    elseif k<=length(quantiles)
        vals=quantile(rhorep,quantiles(k));
    else
        vals=rhoT;
    end
    A = cell(1,1);
    A{1,1}=vals;
    if k==0
        xlswrite(filename,A,1);
    elseif k<=length(quantiles)
        xlswrite(filename,A,num2str(quantiles(k)));
    else
        xlswrite(filename,A,'true values');
    end
end
warning('on','all');
