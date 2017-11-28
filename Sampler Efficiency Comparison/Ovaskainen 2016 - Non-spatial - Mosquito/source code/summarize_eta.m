function [] = summarize_eta(folder,dataset,quantiles,true_values,etarep,nfrep,etaT,nfT,nr)
resultsfolder=strcat(folder,'results\');
warning('off','all');
for ii=1:nr
    nf1=unique(nfrep(:,ii));
    if(length(nf1)==1)
        filename=strcat(resultsfolder,'eta',int2str(ii),dataset,'.xlsx');
        true_values1 = true_values;
        if true_values
            true_values1 = nf1==nfT(ii);
        end
        for k=0:(length(quantiles)+true_values1)
            vals=[];
            for i=1:nf1;
                nmcmc=size(etarep{ii},2)/nf1;
                index=nf1*(0:nmcmc-1)+i;
                tmp=etarep{ii}(:,index);
                cc=corr(tmp);
                change=sum(sign(cc)==-1)>(nmcmc-1)/2;
                tmp(:,change)=-tmp(:,change);
                meta1=mean(tmp');
                if true_values1
                    cc=corr([meta1' etaT{i1}(:,i)]);
                    if(cc(1,2)<0)
                        tmp=-tmp;
                    end
                end
                ny = size(tmp,1);
                if k==0
                    vals=[vals; mean(tmp')];
                elseif k<=length(quantiles)
                    vals=[vals; quantile(tmp',quantiles(k))];
                else
                    vals=[vals; etaT{ii}];
                end
            end
            vals=vals';
            A = cell(ny+1,nf1+1);
            A{1,1}='';
            for i=1:nf1
                A{1,i+1}=strcat('factor ',int2str(i));
            end
            for i=1:ny
                A{i+1,1}=strcat('site ',int2str(i));
            end
            for i=1:ny
                for j=1:nf1
                    A{i+1,j+1}=vals(i,j);
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
