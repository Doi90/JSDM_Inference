function [folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial] = model_definitions();

example = strcat(beta_id,'_',corr_id,'_',cov_id,'_',sim);
basefolder='~/../../data/cephfs/punim0200/MATLAB/examples/'; %BASEFOLDER. STILL TO DETERMINE
%'C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Ovaskainen 2016 - Non-spatial - Frog\examples\'; % SET THE BASEFOLDER HERE
datafolder = strcat(basefolder,example,'data/'); % DATAFOLDER. STILL TO DETERMINE
%'C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Ovaskainen 2016 - Non-spatial - Frog\examples\Frog\','data\');

if strcmp(example,example)
    folder = strcat(basefolder,example,'/');
    traits = false;
    outlierspecies = false;
    dataset = strcat('_',example);
    spatial = false;
end

% PARAMETERS NOT FUNCTIONAL IN THIS VERSION OF THE HMSC - UNDER DEVELOPMENT
speciesX = false; 
phylogeny = false; 
includeXs = false; 

if (~isequal(exist(folder, 'dir'),7))
    mkdir(folder);
end
fol1=strcat(folder,'data\');
if (~isequal(exist(fol1,'dir'),7))
    mkdir(fol1);
end
fol1=strcat(folder,'posteriors\');
if (~isequal(exist(fol1,'dir'),7))
    mkdir(fol1);
end
fol1=strcat(folder,'results\');
if (~isequal(exist(fol1,'dir'),7))
    mkdir(fol1);
end
fol1=strcat(folder,'panels\');
if (~isequal(exist(fol1,'dir'),7))
    mkdir(fol1);
end
