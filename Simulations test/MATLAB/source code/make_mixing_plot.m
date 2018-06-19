function [] = make_mixing_plot(values,valuesT,lab,folder,true_values,saveint)
    thinned=size(values,1);
    mixingfolder=strcat(folder,'panels\mixing_');
    set(0,'DefaultFigureVisible','off');
    figure;
    plot(values);
    lim=axis;
    hold on;
    xlabel(strcat('mcmc round (thinned by',{' '},int2str(saveint),')'));
    tmp=ylabel(lab);
    set(tmp,'Interpreter','none'); 
    tmp=title(strcat('mcmc trace plot for',{' '},lab));   
    set(tmp,'Interpreter','none'); 

    if(true_values)
        plot(1:thinned,(repmat(valuesT(:),1,thinned)));
        mi=min(valuesT(:));
        ma=max(valuesT(:));
        de=(ma-mi);
        axis([lim(1),lim(2),min(lim(3),min(0,mi)-0.2*de-0.2),max(lim(4),max(0,ma)+0.2*de)+0.2]);
    end
    print(strcat(mixingfolder,lab),'-dtiff');
