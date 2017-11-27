function [] =     summarize_ph(folder,dataset,quantiles,true_values,phrep,species,phT)
ns=length(species);
resultsfolder=strcat(folder,'results\');
filename=strcat(resultsfolder,'ph',dataset,'.xlsx');
warning('off','all');
for k=0:(length(quantiles)+true_values)
    if k==0
        vals=mean(phrep);
    elseif k<=length(quantiles)
        vals=quantile(phrep,quantiles(k));
    else
        vals=phT;
    end
    A = cell(ns+1,2);
    A{1,1}='';
    A{1,2}='ph';
    for i=1:ns
        A{i+1,1}=species{i};
    end
    for i=1:ns
        A{i+1,2}=vals(i);
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
