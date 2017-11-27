function [sce_X,sce_Y,sce_ny,sce_pi,sce_np,sce_xy,xy] = load_scenario_data(folder,dataset,scenario,nr,spatial,validation)
datafolder=strcat(folder,'data\');
sce_X = csvread(strcat(datafolder,'X',scenario,'.csv'));
if validation
    sce_Y = csvread(strcat(datafolder,'y',scenario,'.csv'));
else
    sce_Y = [];
end
[sce_ny,tmp]=size(sce_X);
if nr==0
    sce_pi = [];
    sce_np = 0;
    xy = [];
    sce_xy = [];
end
if nr>0
    sce_pi = csvread(strcat(datafolder,'LF_units',scenario,'.csv'));
    [sce_ny nr]=size(sce_pi);
    pio=sce_pi;
    for i=1:nr
        uni=unique(pio(:,i),'stable');
        for j=1:length(uni)
            inds=find(pio(:,i)==uni(j));
            sce_pi(inds,i)=repmat(j,1,length(inds));
        end
    end
    sce_np = max(sce_pi);
end
for i1=1:nr
    xy{i1}=[];
    sce_xy{i1}=[];
    if spatial(i1)
        xy{i1}=csvread(strcat(datafolder,'LF_xy',dataset,'_',int2str(i1),'.csv'));
        sce_xy{i1}=csvread(strcat(datafolder,'LF_xy',scenario,'_',int2str(i1),'.csv'));
    end
end
