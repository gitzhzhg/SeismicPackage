function handle=mystairs(x,y,style)
% Plots stair-step graph; like "stairs, but with an "upstroke"
%
% Written by: E. Rietsch: October 7, 2003
% Last updated: August 29, 2006: Use Matlab-7 version of "stairs"
%
%          handle=mystairs(x,y,style)
% INPUT
% x        edges of stairs 
% y        level of stairs (the number of edges exceeds by 1 the number of
%          levels of the stairs; i.e. length(x)=length(y)+1 )
% style    optional line style of stair-step graph
% OUTPUT
% handle   handle(s) to stair-step graph
%
% EXAMPLE
%          lfigure
%          mystairs(0:11,1:11,':')
%          axis([-0.5,12,0,11.5])


xx=[x(1);x(:)];
yy=[0;y(:);0];

if nargin == 2
   handle=stairs(xx,yy);
else
   handle=stairs(xx,yy,style);
end

if nargout == 0
   clear handle
end
