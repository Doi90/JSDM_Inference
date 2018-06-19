tic

% CODE FOR USER TO MODIFY STARTS - BLOCK 1
true_values = false; % Are the data are simulated so that true values are available for comparison?
posterior_summaries = true; % Posterior summaries outputted to the results folder?
box_plots = true; % Posterior box plots to be outputted to the panels folder
mixing_plots = true; % Mixing plots to be outputted to the panels folder?
quantiles = [0.5 0.025 0.975]; % Which posterior quantiles to be summarized
% CODE FOR USER TO MODIFY ENDS - BLOCK 1

[folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial]=model_definitions();
[ny,nc,ncs,ns,nt,nr,np,dist,X,Xs,T,Y,pi,iQg,detQg,rhopw,iWg,detWg,alphapw] = load_data(folder,dataset,phylogeny,spatial,speciesX,includeXs,traits);
[covariates,scovariates,traitnames,species]=read_cov_trait_sp_names(folder,dataset,nc,ncs,nt,ns,traits,includeXs);
[gammaT,VT,rhoT,betaT,phT,sigmaT,nfT,lambdaT,alphaT,etaT,nfsT,lambdasT,etasT] = read_true_values(folder,dataset,phylogeny,true_values,includeXs,spatial);
[samples,thinning,ny,ns,nc,ncs,nt,nr,outlierspecies,phylogeny,runs,adapt1,adapt2,spatial]=read_metadata(folder,dataset);

% CODE FOR USER TO MODIFY STARTS - BLOCK 2
include_runs=adapt1:runs; % which runs to include
include_sp=1:min(6,ns); % which species to include in the plots (the tables will include all)
include_cs=1:min(6,ncs); % which covariates (to which dimension reduction is to be applied to) to include in the plots (the tables will include all)
% CODE FOR USER TO MODIFY ENDS - BLOCK 2


[gammarep,Vrep,betarep,sigmarep,rhorep,nfrep,phrep,lambdarep,alpharep,etarep,nfsrep,etasrep,lambdasrep] = read_posteriors(folder,dataset,phylogeny,outlierspecies,include_runs,spatial,includeXs);
nmcmc=size(gammarep,1);
if posterior_summaries
    summarize_gamma(folder,dataset,quantiles,true_values,gammarep,covariates,traitnames,gammaT);
    summarize_beta(folder,dataset,quantiles,true_values,betarep,species,covariates,betaT);
    summarize_sigma(folder,dataset,quantiles,true_values,sigmarep,species,sigmaT);
    if outlierspecies
        summarize_ph(folder,dataset,quantiles,true_values,phrep,species,phT);
    end
    if phylogeny
        summarize_rho(folder,dataset,quantiles,true_values,rhorep,rhoT);
    end
    if nr>0
        summarize_lambda(folder,dataset,quantiles,true_values,lambdarep,nfrep,sigmarep,species,lambdaT,sigmaT,nr);
        summarize_eta(folder,dataset,quantiles,true_values,etarep,nfrep,etaT,nfT,nr);
        summarize_alpha(folder,dataset,quantiles,true_values,alpharep,nfrep,alphaT,nfT,spatial);
    end
    if includeXs
        summarize_etas_lambdas_As(folder,dataset,quantiles,true_values,nfsrep,lambdasrep,etasrep,species,scovariates,lambdasT,etasT);
        %summarize_time_series_A(folder,dataset,quantiles,true_values,nfsrep,lambdasrep,etasrep,betarep,species,covariates,scovariates,lambdasT,etasT,betaT);
    end
end

if box_plots | mixing_plots
    if nr>0
        ind=0;
        for k1=1:length(include_sp)-1
            for k2=k1+1:length(include_sp)
                ind=ind+1;
                species_pairs{ind}=strcat(species{include_sp(k1)},'_',species{include_sp(k2)});
            end
        end
    end
    for i=1:nr
        RT{i}=[];
        if true_values
            RR=corrcov(lambdaT{i}(:,include_sp)'*lambdaT{i}(:,include_sp)+diag(sigmaT(include_sp)));
            fR=[];
            for k1=1:length(include_sp)-1
                for k2=k1+1:length(include_sp)
                    fR=[fR; RR(k1,k2)];
                end
            end
            RT{i}=fR;
        end
        lambda1=lambdarep{i};
        nf1=nfrep(:,i);
        omega1=[];
        for j=1:length(nf1)
            lambda2=lambda1(include_sp,1:nf1(j));
            RR=corrcov(lambda2*lambda2'+diag(sigmarep(j,include_sp)));
            fR=[];
            for k1=1:length(include_sp)-1
                for k2=k1+1:length(include_sp)
                    fR=[fR; RR(k1,k2)];
                end
            end
            omega1=[omega1, fR];
            lambda1=lambda1(:,nf1(j)+1:size(lambda1,2));
        end
        R{i}=omega1';
    end
    
    beta1 = zeros(size(betarep,1),nc*length(include_sp));
    for i=1:size(betarep,1)
        lbeta=reshape(betarep(i,:),nc,ns);
        lbeta=lbeta(:,include_sp);
        beta1(i,:) = lbeta(:);
    end
    if true_values
        betaT1=betaT(:,include_sp);
    else
        betaT1=[];
    end
    
    if mixing_plots
        make_mixing_plot(gammarep,gammaT,strcat('gamma',dataset),folder,true_values,thinning);
        make_mixing_plot(Vrep,VT,strcat('V',dataset),folder,true_values,thinning);
        make_mixing_plot(beta1,betaT1,strcat('beta',dataset),folder,true_values,thinning);
        if sum(dist==2)<ns
            make_mixing_plot(log10(sigmarep),log10(sigmaT),strcat('log10 sigma',dataset),folder,true_values,thinning);
        end
        if nr>0
            make_mixing_plot(nfrep,nfT,strcat('nf',dataset),folder,true_values,thinning);
        end
        if phylogeny
            make_mixing_plot(rhorep,rhoT,strcat('Prho',dataset),folder,true_values,thinning);
        end
        for i=1:nr
            make_mixing_plot(R{i},RT{i},strcat('R',int2str(i),dataset),folder,true_values,thinning);
        end
        for i=1:nr
            nfi=unique(nfrep(:,i));
            if(length(nfi)==1 & spatial(i))
                make_mixing_plot(reshape(alpharep{i},nmcmc,nfi),alphaT{i},strcat('alpha',int2str(i),dataset),folder,true_values,thinning);
            end
        end
    end
    
    if includeXs
        nfis=unique(nfsrep);
        if(length(nfis)==1)
            lambdas1 = zeros(nmcmc,nfis*length(include_sp));
            lambdasA=lambdasrep;
            etas1 = zeros(nmcmc,nfis*length(include_cs));
            etasA=etasrep;
            Am1=zeros(nmcmc,length(include_sp)*length(include_cs));
            for i=1:nmcmc
                lambdas3=lambdasA(1:nfis,:);
                lambdas2=lambdas3(:,include_sp);
                lambdas1(i,:)=lambdas2(:);
                lambdasA=lambdasA(nfis+1:size(lambdasA,1),:);
                etas3=etasA(1:nfis,:);
                etas2=etas3(:,include_cs);
                etas1(i,:)=etas2(:);
                etasA=etasA(nfis+1:size(etasA,1),:);
                Am2=etas2'*lambdas2;
                Am1(i,:)=Am2(:);
            end
            if true_values
                lambdasT1=lambdasT(:,include_sp);
                etasT1=etasT(include_cs,:)';
                AmT1=etasT1'*lambdasT1;
                if(corr(mean(lambdas1)',lambdasT1(:))<0)
                    lambdasT1=-lambdasT1;
                    etasT1=-etasT1;
                end
                AmT=etasT*lambdasT;
            else
                lambdasT1=[];
                etasT1=[];
                AmT1=[];
            end
            if mixing_plots
                make_mixing_plot(reshape(etasrep',nfis*ncs,nmcmc)',etasT(:),'etas',folder,true_values,thinning);
                make_mixing_plot(lambdas1,lambdasT1,'lambdas',folder,true_values,thinning);
                make_mixing_plot(Am1,AmT1,'As',folder,true_values,thinning);
            end
            if box_plots
                make_box_plot(lambdas1,lambdasT1,strcat('lambdas',dataset),' summarized scovariate',{'1','2','3','4'},'species: ',species(include_sp),folder,nfis,length(include_sp),true_values);
                make_box_plot(etas1,etasT1,strcat('etas',dataset),' summarized scovariate',{'1','2','3','4'},'scovariate: ',scovariates(include_cs),folder,nfis,length(include_cs),true_values);
                make_box_plot(Am1,AmT1,strcat('As',dataset),'scovariate: ',scovariates(include_cs),' species: ',species(include_sp),folder,length(include_cs),length(include_sp),true_values);
            end
        end
    end
    
    if box_plots
        make_box_plot(gammarep,gammaT,strcat('gamma',dataset),'trait',traitnames,'covariate: ',covariates,folder,nt,nc,true_values);
        make_box_plot(Vrep,VT,strcat('V',dataset),'covariate',covariates,'covariate: ',covariates,folder,nc,nc,true_values);
        make_box_plot(beta1,betaT1,strcat('beta',dataset),'covariate',covariates,'species: ',species(include_sp),folder,nc,length(include_sp),true_values);
        if sum(dist==2)<ns
            make_box_plot(log10(sigmarep),log10(sigmaT),strcat('log10(sigma)',dataset),'species',species,'log(sigma)',{''},folder,ns,1,true_values);
        end
        if phylogeny
            make_box_plot(rhorep,rhoT,strcat('Prho',dataset),'',{''},'Prho',{''},folder,1,1,true_values);
        end
        for i=1:nr
            make_box_plot(R{i},RT{i},strcat('R',int2str(i),dataset),'species pair',species_pairs,strcat('R',int2str(i)),{''},folder,length(species_pairs),1,true_values);
        end
        
        for i1=1:nr
            if spatial(i1)
                xy1=csvread(strcat(folder,'data\LF_xy',dataset,'_',int2str(i1),'.csv'));
                if true_values
                    for i=1:nfT
                        clabT{i}=strcat('etaT(',int2str(i1),',',int2str(i),')');
                    end
                    make_map_plot(xy1,etaT{i1},strcat('etaT_',int2str(i1),dataset),clabT,folder);
                end
                nfi=unique(nfrep(:,i1));
                if(length(nfi)==1)
                    make_box_plot(reshape(alpharep{i1},nmcmc,nfi),alphaT{i1},strcat('alpha',int2str(i1),dataset),'factor',{'1','2','3','4'},'alpha',{''},folder,nfi,1,true_values);
                    meta=[];
                    for i=1:nfi;
                        index=nfi*(0:nmcmc-1)+i;
                        tmp=etarep{i1}(:,index);
                        cc=corr(tmp);
                        change=sum(sign(cc)==-1)>(nmcmc-1)/2;
                        tmp(:,change)=-tmp(:,change);
                        meta1=mean(tmp');
                        clab{i}=strcat('eta(',int2str(i1),',',int2str(i),')');
                        if true_values & i<=nfT(i1)
                            cc=corr([meta1' etaT{i1}(:,i)]);
                            if(cc(1,2)<0)
                                meta1=-meta1;
                            end
                            clab{i}=strcat(clab{i},': cor = ',num2str(abs(cc(1,2))));
                        else
                            clab{i}=strcat(clab{i},': cor = NA');
                        end
                        meta=[meta; meta1];
                    end
                    meta=meta';
                    make_map_plot(xy1,meta,strcat('eta_',int2str(i1),dataset),clab,folder);
                end
            end
        end
    end
end

save_results(folder,dataset,0,betarep,sigmarep,gammarep,Vrep,rhorep,nfrep,lambdarep,alpharep,etarep,nfsrep,lambdasrep,etasrep,phrep,phylogeny,outlierspecies,includeXs,spatial);

toc