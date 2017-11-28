function [] = make_map_plot(xy,values,lab,clab,folder)
    mapfolder=strcat(folder,'panels\map_');
    spatdim=size(xy,2);
    nh=size(values,2);
    nhx=ceil(sqrt(nh));
    nhy=ceil(nh/nhx);
    figure;
    for i=1:nh;
        subplot(nhy,nhx,i);
        if spatdim==2
            scatter(xy(:,1),xy(:,2),[],values(:,i),'filled');
            xlabel('x-coordinate');
            ylabel('y-coordinate');
            axis square
        end
        if spatdim==1
            scatter(xy,values(:,i),'filled');
            xlabel('time');
        end
        tmp=title(clab{i});   
        set(tmp,'Interpreter','none'); 
    end
    print(strcat(mapfolder,lab),'-dtiff');
