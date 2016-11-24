function [e,evec]=rcerror(rc1,rc2,thresh,flag,p)
%
% [e,evec]=rcerror(rc1,rc2,thresh,flag,p)
%
%
% rc1 ... test rcs
% rc2 ... comparison rcs
% thresh ... threshold of rc to worry about. .1 means only use all rcs greater
%       than .1 times the largest abs rc 
% ********* default =.1 *******
% flag ... 1 normalize errors by ./abs(amp2)
%          2 normalize errors by /mean(abs(amp2))
% ******** default = 2 *********
% p ... 1 means L1 norm, 2 means L2 norm
% ******** default =1 **********
% 

if(nargin<3)
    thresh=.1;
end
if(nargin<4)
    flag=2;
end
if(nargin<5)
    p=1;
end

rmax=max(abs(rc2));
ind=find(abs(rc2)>thresh*rmax);

if(p==1)
    if(flag==1)
        evec=abs(rc1(ind)-rc2(ind))./abs(rc2(ind));
    else
        evec=abs(rc1(ind)-rc2(ind))./mean(abs(rc2(ind)));
    end
    e=mean(evec);
else
    if(flag==1)
        evec=(rc1(ind)-rc2(ind)).^2./rc2(ind).^2;
    else
        evec=(rc1(ind)-rc2(ind)).^2./mean(rc2(ind).^2);
    end
    e=sqrt(mean(evec));
end

