function [f0,V0,Ugamma,mgamma,asigma,bsigma,nur,a1r,b1r,a2r,b2r,nus,a1s,b1s,a2s,b2s,nu]=define_priors(ns,nc,nt,phylogeny)
    nu = 4; % gamma parameter for outlierspecies

    nur = 3;                                   % gamma hyperparameters for psijh
    a1r = 50.; b1r = 1;                         % gamma hyperparameters for delta_ 1
    a2r = 50.; b2r = 1;                         % gamma hyperparameters delta_h, h >= 2

    nus = 3;                                   % gamma hyperparameters for psijh
    a1s = 50.; b1s = 1;                         % gamma hyperparameters for delta_ 1
    a2s = 50.; b2s = 1;                         % gamma hyperparameters delta_h, h >= 2
    
    asigma = 1.0*ones(ns,1);                   % gamma hyperparameters sigma
    bsigma = 0.3*ones(ns,1);

    f0 = nc + 1;                               
    V0 = eye(nc);
    Ugamma = eye(nc*nt);                               
    mgamma = zeros(nc*nt,1);
