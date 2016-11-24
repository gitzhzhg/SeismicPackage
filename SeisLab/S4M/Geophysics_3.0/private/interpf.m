function ynew=interpf(yold,dxold,dxnew)
% Function interpolates in the frequency domain; 
% appends data to mitigate "edge effects" prior to FFT, and removes them afterwards
% assumes that "dxold" is integer multiple of "dxnew" or vice versa
% Written by: E. Rietsch: February 14, 2001
% Last updated: February 24, 2001: Complete overhaul of function
%
%          ynew=interpf(yold,dxold,dxnew)
% INPUT
% yold     original data (can be matrix)
% dxold    original sample interval
% dxnew    new sample interval
% OUTPUT
% ynew     resampled data

[nold,ntr]=size(yold);
temp=[yold;yold(end,:)*0.5;zeros(nold-2,ntr);yold(1,:)*0.5];

if dxold > dxnew
  frac=dxold/dxnew;
  if abs(round(frac)-frac) > 1.0e6*eps
     disp([' "interpf" requires that the old sample interval (',num2str(dxold),') is an'])
     disp([' integer multiple of the new sample interval (',num2str(dxnew),') or vice versa'])
     error(' Abnormal termination')
  else
     frac=round(frac);
  end
  ynew=interp_periodic(temp,frac,1);
  nnew=nold*frac;

elseif dxold < dxnew
  frac=dxnew/dxold;
  if abs(round(frac)-frac) > 1.0e6*eps
     disp([' "interpf" requires that the new sample interval (',num2str(dxnew),') is an'])
     disp([' integer multiple of the old sample interval (',num2str(dxold),') or vice versa'])
     error(' Abnormal termination')
  else
     frac=round(frac);
  end
  ynew=interp_periodic(temp,1,frac);
  nnew=fix(nold/frac);

else
  ynew=yold;
  nnew=nold;

end

ynew=ynew(1:nnew,:);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ynew=interp_periodic(yold,dxold,dxnew)
% Function interpolates in the frequency domain
%
%          ynew=interp_periodic(yold,dxold,dxnew)
% INPUT
% yold     original data (can be matrix)
% dxold    original sample interval
% dxnew    new sample interval
% OUTPUT
% ynew     resampled data

[nold,ntr]=size(yold);

     if dxold > dxnew
if mod(dxold,dxnew) == 0
   xnew=0:dxnew:nold*dxold-dxnew;
else
   xnew=0:dxnew:nold*dxold-dxnew;
end
nnew=length(xnew);
fyold=fft(yold);
fynew=zeros(nnew,ntr);
if ~mod(nold,2)
  nh=nold/2+1;
  fyold(nh)=fyold(nh)*0.5;
else
  nh=(nold+1)/2;
end
fynew(1:nh,:)=fyold(1:nh,:);
fynew(end-nh+2:end,:)=fyold(end-nh+2:end,:);
ynew=real(ifft(fynew))*(dxold/dxnew);

     elseif dxold == dxnew
ynew=yold;

     else     % dxold < dxnew
xnew=0:dxnew:nold*dxold*(1-1.0e6*eps);
nnew=length(xnew);
yold=fft(yold);
ynew=zeros(nnew,ntr);
if ~mod(nnew,2)
   nh=nnew/2+1;
   yold(end-nh+2)=yold(end-nh+2)*2;
else
   nh=(nnew+1)/2;
end
ynew(1:nh,:)=yold(1:nh,:);
ynew(end-nh+2:end,:)=yold(end-nh+2:end,:);
ynew=real(ifft(ynew))*(dxold/dxnew);

     end
