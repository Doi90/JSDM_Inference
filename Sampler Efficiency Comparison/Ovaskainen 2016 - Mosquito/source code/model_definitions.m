function [folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial] = model_definitions();

basefolder='C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Sampler Efficiency Comparison\Ovaskainen 2016 - Mosquito\examples\'; % SET THE BASEFOLDER HERE
datafolder = strcat('C:\Users\davidpw\Dropbox\JSDM\JSDM Comparison Compiled\Sampler Efficiency Comparison\Ovaskainen 2016 - Mosquito\examples\Mosquito\','data\');
example = 'Mosquito';

if strcmp(example,'Mosquito')
    folder = strcat(basefolder,'Mosquito','\');
    traits = false;
    outlierspecies = false;
    dataset = '_Mosquito';
    spatial = true;
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
