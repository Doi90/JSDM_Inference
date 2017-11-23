function alpha = update_alpha(eta,alpha,nf,pi,spatial,iWg,detWg,alphapw)
if min(pi)<0
    np = [];
    nr = 0;
else
    [ny nr]=size(pi);
end
for i1=1:nr
    if spatial(i1)==1
        iWg1 = iWg{i1};
        detWg1 = detWg{i1};
        alphapw1 = alphapw{i1};
        alpha1=alpha{i1};
        eta1=eta{i1};
        for h=1:nf(i1);
            tmp = [];
            for ag = 1:length(detWg1)
                tmp = [tmp; eta1(:,h)'*iWg1(:,:,ag)*eta1(:,h)];
            end
            like = log(alphapw1(:,2))-1/2*detWg1-(1/2)*tmp;
            like=like-max(like);
            like = exp(like);
            like = like/sum(like);
            alpha1(h)=randsample(length(like),1,true,like);
        end
        alpha{i1}=alpha1;
    end
end
