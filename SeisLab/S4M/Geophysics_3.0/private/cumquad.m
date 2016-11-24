function ci = cumquad(y,x)
% Function computes the numerical approximation to the indefinite 
% integral y dx (corresponding to cumsum)
%
% Written by: E. Rietsch: September 22, 1999
% Last updated: September 28, 2006: Faster processing with one input argument
%
%       ci = cumquad(y,x)
% INPUT 
% y     ordinates 
% x     abscissas; if only one input argument is given then x=1:1:size(y,1);
%       if given, x must have the same number of rows as y
%       and either the same number of columns or one column
%       (see also quad2)
% OUTPUT
% ci    cumulative integral

[ny,my]=size(y);
itransp=0;
if ny == 1  &&  my > 1
   itransp=1;
   y=y(:);
   [ny,my]=deal(my,ny);
   if nargin == 2
      x=x(:);
   end
end

dy=(y(1:end-1,:)+y(2:end,:))*0.5;

if nargin == 2
   [nx,mx]=size(x);

   if nx ~= ny
      disp([size(x), size(y)])
      error('Input arrays have incompatible dimensions.')
   end

   dx=diff(x);
   if mx == 1  &&  my ~= 1
      dx=dx(:,ones(my,1));
   elseif mx ~= my,
      size(x), size(y)      
      error('Input arrays have incompatible dimensions.')
   end

   ci=[zeros(1,my);mycumsum(dx.*dy)];

else
   ci=[zeros(1,my);mycumsum(dy)];
end


if itransp
   ci=ci(:);
end
