function [] = summarize_alpha(folder,dataset,quantiles,true_values,alpharep,nfrep,alphaT,nfT,spatial)
nr=length(spatial);
resultsfolder=strcat(folder,'results\');
warning('off','all');
for ii=1:nr
    nf1=unique(nfrep(:,ii));
    if(length(nf1)==1 & spatial(ii))
        filename=strcat(resultsfolder,'alpha',int2str(ii),dataset,'.xlsx');
        alpha1=reshape(alpharep{ii},size(alpharep{ii},1)/nf1,nf1);
        true_values1 = true_values;
        if true_values
            true_values1 = nfT(ii)==nf1;
        end
        for k=0:(length(quantiles)+true_values1)
            if k==0
                vals=mean(alpha1);
            elseif k<=length(quantiles)
                vals=quantile(alpha1,quantiles(k));
            else
                vals=alphaT{ii};
            end
            A = cell(nf1+1,2);
            A{1,1}='';
            A{1,2}='spatial scale';
            for i=1:nf1
                A{i+1,1}=strcat('factor ',int2str(i));
            end
            for i=1:nf1
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
    end
end
