function trend=repeated_median_trend(vector,x)
% Compute the trend by means repeated median of the gradients of the vector samples
% Ref: Siegel, A.F. (1982), "Robust Regression using Repeated Medians."
%                            Biometrika. 69, pp 242-244
% Written by: E. R.: March 22, 2005
% Last updated:
%
%            med=repeated_median_trend(vector,x)
% INPUT
% vector     vector whose trend is to be calculated
% x          coordinates in case the values are not equally spaced
%            Default: 1:length(vector)
% OUTPUT
% trend      trend


n=length(vector);
grads=zeros(n-1,1);

if nargin == 2
   vector=vector(:);
   x=x(:);
   for ii=1:n-1
      grads(ii)=median((vector(ii+1:end)-vector(ii))./(x(ii+1:end)-x(ii)));
   end
else
   vector=vector(:)';
   for ii=1:n-1
      grads(ii)=median((vector(ii+1:end)-vector(ii))./(1:n-ii));
   end
end
trend=median(grads);
         
