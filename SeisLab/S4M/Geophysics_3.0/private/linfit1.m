function [a,b]=linfit1(x,y,alpha,nd)
% Function fits a straight line to data whose x-coordinates 
% and y-coordinates are in arrays x and y, respectively. 
% The slope b of the straight line is determined as a weighted average 
% of the slopes between differnt abscissa values. The algorithm start with the
% slope between the last and the first abscissa and moves inward from there on.
% There are n(n-1)/2 possible slopes for n abscissa/ordinate pairs, but the program
% will use at most 4950 slopes or --- if nd is given --- nd(nd+1)/2 slopes using 
% the outermost samples
%
%        [a,b]=linfit1(x,y,alpha,nd)
% INPUT
% x,y    abscissas and ordinates
% alpha  drop alpha*100 % largest and smallest slopes (optional), 
%        default: 0 (i.e. keep all)); 0 <= alpha < 0.5
% nd     optional parameter (nd < 100); see above
%        values of nd less than zero or greater than 99 are ignored.
%        The vectors x and y may contain NaN which are eliminated 
%        before the calculation begins.
% OUTPUT
% a,b    The output variables a, b are the coefficients in the
%        least-squares fit y = a + bx.

x=x(:); 
y=y(:);

if nargin <4
  nd=99;
else
  if nd > 99, nd=99; end
end

if nargin < 3
  alpha=0;
else
  if alpha >= 0.5  || alpha < 0
     alpha =0;
     alert('Parameter alpha changed to 0')
  end
end

% 	Eliminate NaNs
index=find(~isnan(x));
x1=x(index); y1=y(index);
index=find(~isnan(y1));
x1=x1(index); y1=y1(index);

%   	Sort abscissas in ascending order
[x1,idx]=sort(x1); 
y1=y1(idx);

%   	Compute up to nd(nd+1)/2 possible differences beginning 
%   	at the smallest and largest abscissas
n=length(x1);
if n <= nd+1
   nd=n-1;
end

nn=nd*(nd+1)/2;

dx=zeros(nn,1); dy=zeros(nn,1);
iii=1;
for jj=1:nd
   dx(iii:iii+jj-1)=x1(end-jj+1:end)-x1(1:jj);
   dy(iii:iii+jj-1)=y1(end-jj+1:end)-y1(1:jj);
   iii=iii+jj;
end

if alpha > 0
  drop=floor(length(dx)*alpha);
  [dummy,idx]=sort(dy./dx);
  idx=idx(drop+1:end-drop);
  b=(dx(idx)'*dy(idx))/(dx(idx)'*dx(idx));
else
  b=(dx'*dy)/(dx'*dx);
end     
a=mean(y1-b*x1);

