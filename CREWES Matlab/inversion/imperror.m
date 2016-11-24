function [e,evec]=imperror(imp1,imp2,flag,p)
% 
% [e,evec]=imperror(imp1,imp2,flag,p)
%
% imp1 ... impedance with errors
% imp2 ... reference impedance (no errors)
% flag ... 1 normalize errors by ./imp2
%          2 normalize errors by /mean(amp2)
% ****** default = 2 *******
% p ... 1 compute L1 errors like e=mean(abs((imp1-imp2)./imp2))
%       2 compute L2 errors e=sqrt(mean((imp2-imp1).^2./imp2.^2));
% ******* default =1 *******
%      

if(nargin<4)
    p=1;
end
if(nargin<3)
    flag=2;
end
evec=[];
if(p==1)
    if(flag==1)
        ev=abs((imp2-imp1)./imp2);
    else
        ev=abs((imp2-imp1)./mean(imp2));
    end
    
    e=mean(ev);
else
    if(flag==1)
        ev=(imp2-imp1).^2./imp2.^2;
    else
        ev=(imp2-imp1).^2./mean(imp2.^2);
    end
    e=sqrt(mean(ev));
end
if(nargout>1)
    evec=ev;
end