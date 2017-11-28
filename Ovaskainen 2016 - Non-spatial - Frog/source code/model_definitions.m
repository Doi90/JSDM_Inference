function [folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial] = model_definitions();

basefolder='C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Ovaskainen 2016 - Non-spatial - Frog\examples\'; % SET THE BASEFOLDER HERE
datafolder = strcat('C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Ovaskainen 2016 - Non-spatial - Frog\examples\Frog\','data\');
example = 'Frog';

if strcmp(example,'Frog')
    folder = strcat(basefolder,'Frog','\');
    traits = false;
    outlierspecies = false;
    dataset = '_Frog';
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
