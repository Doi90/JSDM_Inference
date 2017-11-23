function [folder,dataset,traits,speciesX,phylogeny,includeXs,outlierspecies,spatial] = model_definitions();

basefolder='C:\Users\davidpw\Desktop\JSDM Comparison Compiled\Ovaskainen 2016 - Butterfly\examples\'; % SET THE BASEFOLDER HERE
datafolder = strcat('C:\Users\davidpw\Desktop\JSDM Comparison Compiled\Ovaskainen 2016 - Butterfly\examples\Butterfly\','data\');
example = 'Butterfly';

if strcmp(example,'Butterfly')
    folder = strcat(basefolder,'Butterfly','\');
    traits = false;
    outlierspecies = false;
    dataset = '_Butterfly';
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
