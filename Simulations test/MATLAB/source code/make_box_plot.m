function [] = make_box_plot(values,valuesT,lab,xlab,xticks,ylab,yticks,folder,ncol,nrow,true_values)
    plotfolder=strcat(folder,'panels\boxplot_');
    figure;
    nboxes=nrow*ncol;
    cols=ceil(sqrt(nboxes)/ncol);
    rows=ceil(nrow/cols);
    for i=1:nrow
        h=subplot(rows,cols,i);
        p=get(h,'pos');
        boxplot(values(:,(i-1)*ncol+1:(i-1)*ncol+ncol));
        lim=axis;
        hold on;
        tmp=ylabel(lab);
        set(tmp,'Interpreter','none');
        %set(gca,'xtick',1:ncol,'xticklabel',xticks);
        set(gca,'xtick',1:ncol);
        % set(gca,'xticklabel',xticks);
        tmp=title(strcat(ylab,yticks{i})); 
        set(tmp,'Interpreter','none'); 
        if(i==nrow)
           tmp=xlabel(xlab);
           set(tmp,'Interpreter','none'); 
        end
        plot([0.5 ncol+0.5],[0 0],'blue');
        if true_values
            scatter(1:length(valuesT(:,i)),valuesT(:,i),'filled','red');
            mi=min(valuesT(:,i));
            ma=max(valuesT(:,i));
            de=(ma-mi);
            axis([0.5,ncol+0.5,min(lim(3),min(0,mi)-0.2*de-0.2),max(lim(4),max(0,ma)+0.2*de)+0.2]);
        end
    end
    print(strcat(plotfolder,lab),'-dtiff');
 