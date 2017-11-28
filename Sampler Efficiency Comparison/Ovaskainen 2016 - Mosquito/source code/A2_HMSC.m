clearvars;
tic

% CODE FOR USER TO MODIFY STARTS
runs = 5; % how many times the the nrun iterations are done; results saved after each one put to a new folder; adaptation is done only during the first one
samples = 2500; % how many posterior samples are stored for each run
thinning = 1; % how many times the MCMC is run before one sample is saved
adapt = [2 0]; % adapt = [x y ] means that continue adaptation for the number of latent factors until iteration y (out of thinned*saveint) of run x;
fix_nf = false; % set to true if the number of latent factors is not to be estimated 
% CODE FOR USER TO MODIFY ENDS

[folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial]=model_definitions();
[ny,nc,ncs,ns,nt,nr,np,dist,X,Xs,T,Y,pi,iQg,detQg,rhopw,iWg,detWg,alphapw] = load_data(folder,dataset,phylogeny,spatial,speciesX,includeXs,traits);
[f0,V0,Ugamma,mgamma,asigma,bsigma,nur,a1r,b1r,a2r,b2r,nus,a1s,b1s,a2s,b2s,nu]=define_priors(ns,nc,nt,phylogeny);
csvwrite(strcat(folder,'posteriors\metadata',dataset,'.csv'),[samples thinning ny ns nc ncs nt nr outlierspecies phylogeny runs adapt(1) adapt(2) spatial]);
[beta,gamma,iV,sigma,rho,ph,z,nf,lambda,eta,delta,psijh,alpha,nfs,lambdas,etas,deltas,psijhs]=compute_initial_values(X,Xs,Y,T,pi,dist,phylogeny,V0,mgamma,nur,a1r,b1r,a2r,b2r,nus,a1s,b1s,a2s,b2s,speciesX,spatial);

fprintf('Sampling starts.\n');
for run = 1:runs
    fprintf(strcat('SAVE REPLICATE:',int2str(run),'\n'));
    [betarep,gammarep,Vrep,rhorep,sigmarep,nfrep,lambdarep,phrep,etarep,alpharep,nfsrep,lambdasrep,etasrep] = initialize_output(samples,nc,ncs,ns,nt,nr);
    for mcmc = 1:samples*thinning
        lambda = update_lambda(X,Xs,z,beta,sigma,eta,lambda,etas,lambdas,delta,psijh,pi,nf,spatial,speciesX,includeXs);
        eta = update_eta(z,X,Xs,beta,sigma,eta,alpha,lambda,etas,lambdas,nf,pi,spatial,iWg,speciesX,includeXs);
        alpha = update_alpha(eta,alpha,nf,pi,spatial,iWg,detWg,alphapw);
        z = update_z(X,Xs,beta,eta,lambda,etas,lambdas,Y,pi,dist,sigma,speciesX);
        sigma = update_sigma(X,Xs,beta,eta,lambda,etas,lambdas,pi,dist,asigma,bsigma,z,spatial,speciesX,includeXs);
        beta = update_beta(X,Xs,z,pi,eta,lambda,etas,lambdas,gamma,sigma,T,ph,iV,rho,phylogeny,iQg,detQg,outlierspecies,spatial,speciesX,includeXs);
        [gamma,iV] = update_gamma_V(T,beta,gamma,ph,rho,V0,f0,Ugamma,mgamma,phylogeny,iQg,outlierspecies);
        if outlierspecies
            ph = update_ph(T,beta,iV,ph,gamma,nu);
        end
        [psijh,delta] = update_lambda_priors(nf,nur,a1r,a2r,b1r,b2r,psijh,delta,lambda);
               
        if  (run<adapt(1)) || ((run==adapt(1)) && mcmc<=adapt(2) && ~fix_nf)
           [nf,lambda,eta,alpha,psijh,delta] = update_nf(nf,ns,mcmc,np,nur,a2r,b2r,lambda,eta,alpha,psijh,delta);
        end
        [betarep,gammarep,sigmarep,rhorep,Vrep,lambdarep,etarep,alpharep,nfrep,phrep,lambdasrep,etasrep,nfsrep] = add_to_output(mcmc,thinning,beta,gamma,sigma,rhopw,rho,iV,lambda,eta,alpha,ph,nf,lambdas,etas,nfs,betarep,gammarep,sigmarep,rhorep,Vrep,lambdarep,etarep,alpharep,nfrep,phrep,lambdasrep,etasrep,nfsrep,alphapw,phylogeny,spatial);
    end
    save_results(folder,dataset,run,betarep,sigmarep,gammarep,Vrep,rhorep,nfrep,lambdarep,alpharep,etarep,nfsrep,lambdasrep,etasrep,phrep,phylogeny,outlierspecies,includeXs,spatial);
end

toc