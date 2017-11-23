function z =  update_z(X,Xs,beta,eta,lambda,etas,lambdas,Y,pi,dist,sigma,speciesX)
if speciesX
    [ny nc]=size(X{1});
else
    [ny nc]=size(X);
end
ns = length(dist);
if min(pi)<0
    nr = 0;
else
    nr = size(pi,2);
end

if speciesX
    Ez = zeros(ny,ns);
    for i=1:ns
        Ez(:,i)=X{i}*beta(:,i);
    end
else
    Ez = X*beta;
end
if(size(Xs,1)>0)
    Ez = Ez+Xs*etas*lambdas;
end
for i1=1:nr
    eta1=eta{i1};
    lambda1=lambda{i1};
    Ez = Ez+eta1(pi(:,i1),:)*lambda1;
end
[ny ns]=size(Ez);

low = -Inf*ones(ny,ns);
high = Inf*ones(ny,ns);
cut=-Ez;
low(Y==1)=cut(Y==1);
high(Y==0)=cut(Y==0);

missing=isnan(Y);
low(missing)=-Inf;
high(missing)=Inf;

sel=(dist==2)
mu = zeros(ny,sum(sel));
dsigma=sqrt(diag(sigma));
dsi=dsigma(sel);
si = repmat(dsi,1,ny)';
eps = tnormrnd(mu,si,low(:,sel),high(:,sel));
[row,col]=find(eps==Inf);
for  j=1:length(row)
    eps(row(j),col(j))=low(row(j),col(j))+0.5;
end
[row,col]=find(eps==-Inf);
for  j=1:length(row)
    eps(row(j),col(j))=high(row(j),col(j))-0.5;
end

z(:,sel)=Ez(:,sel)+eps;


