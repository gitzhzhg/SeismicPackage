function [a,b]=l1_slope_intercept(x,y,alpha)
% Compute an L1 approximation to intercept "a" and slope "b" 
%      y = a + b x
% 
% Written by: E. Rietsch: August 8, 2003
% Last updated:
%
%       [a,b]=l1_slope_intercept(x,y);
% INPUT
% x     abscissas
% y     ordinates
% alpha   a trim parameter; determines how many slopes are computed (slopes 
%       are computed between the first "ia" and the last "ia" abscissa and 
%       ordinate values where ia=0.5*length(x)*alpha)
% OUTPUT
% a     intercept
% b     slope

if nargin < 3
   alpha=0.25;
end

nx=length(x);
nxh=nx/2;

ia=round(max(1,nxh*alpha));
ie=round(nxh*(1-alpha));
if ia > ie 
   error('Parameter "alpha" must be between 0 and 0.5')
end

nxh=round(nxh);
%	Compute slopes 
temp=(y(end-nxh+1:end)-y(1:nxh))./(x(end-nxh+1:end)-x(1:nxh));

temp=sort(temp);
b=mean(temp(ia:ie));

temp=sort(y-b*x);
a=mean(temp(ia:ie));
