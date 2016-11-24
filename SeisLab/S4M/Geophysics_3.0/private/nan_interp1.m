function yi=nan_interp1(x,y,xi,method,null)
% Interpolate function y(x) with NaN's. No extrapolation.
% No interpolation across gaps with valid data on both sides.
% "yi(xi)" is set to NaN if "y(x)" on either side is NaN.
% See also: fill_nan_gaps
%
% Written by: E. Rietsch: February 18, 2007
% Last updated: December 7, 2007: bug fix
%
%         yi=nan_interp1(x,y,xi,method)
% INPUT
% x       vector of abscissas
% y       vector or matrix of ordinates
% xi      new abscissas
% method  interpolation method; see Matlab function "interp1"
%         Default: method='linear'
% null    Value to assign to missing data that have valid data on both
%         sides (i.e. not initial or final missing values)
% OUTPUT
% yi      interpolated matrix "y"
%
% EXAMPLE
%         x=1:11;
%         y=x;
%         y(5)=NaN;
%         xi=0:0.5:11;
%         yi=nan_interp1(x),y,xi,'v5cubic')'


if nargin == 3
   method='linear';
end

[nsamp,ntr]=size(y);

if nsamp==1
   ntr=1;
   y=y(:);
   flip=true;
else
   flip=false;
end

yi=NaN(length(xi),ntr);
index=find(xi >= x(1) & xi <= x(end));
ia=index(1);
ie=index(end);
xx=xi(ia:ie);

for ii=1:ntr
   bool=~isnan(y(:,ii));
   if all(bool)	        % No NaN's
      yi(ia:ie,ii)=interp1(x,y(:,ii),xx,method);
   
   elseif ~any(bool)	% All y(:,ii) are NaN
      % Do nothing

   else
      idx=find(bool);
      idx1=find(xx >= x(idx(1))  &  xx <= x(idx(end)));
      if length(idx) < 3
         yi(ia+idx1(1)-1:ia+idx1(end)-1,ii)=interp1(x(idx),y(idx,ii), ...
                                            xx(idx1(1):idx1(end)));
      else
         yi(ia+idx1(1)-1:ia+idx1(end)-1,ii)=interp1(x(idx),y(idx,ii), ...
                                            xx(idx1(1):idx1(end)),method);
      end

      %		Remove values associated with NaN's in "y" that were
      %         "skipped over" in the above interpolation by checking 
      %         which new abscissas fall between old abscissas where at
      %         least one ordinate is a null value.
      binindex=find_bins(xx(idx1),x);
      idx2=find(~bool);
      idx2(idx2<idx1(1) | idx2>idx1(end))=[];
      for jj=1:length(idx2)
         yi(binindex >= idx2(jj) & binindex <= idx2(jj)+1,ii)=null;
      end
   end
end

%	If "y" is a row vector "yi" should also be a row vector
if flip
   yi=yi.';
end
