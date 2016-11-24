function [mn,stdev]=nan_mean(array,dim)
% Function computes mean and standard deviation of the input array in 
% dimension dim; the array is assumed to contain NaNs which are excluded from
% the calculation.
% 
% Written by: E. Rietsch:
% Last updated: June 2, 2007: Generalize to more than two matrix dimensions
%
%          [mn,stdev]=nan_mean(array,dim);
% INPUT
% array    matrix
% dim      dimension of the matrix along which the mean and the standard 
%          deviation are to be computed; optional; default: dim=1
% OUTPUT
% mn       computed mean along the dimension specified
% stdev    standard deviation along the dimension specified


dims=size(array);
ldims=length(dims);

if nargin == 1
   dim=1;
else
   if dim > ldims
      error('Dimension along which to compute the mean cannot exceed the dimension of the matrix.')
   end
end

if ldims >= 3  && dims(3) > 1
   array=shiftdim(array,dim-1);
   array=reshape(array,dims(dim),[]);
   dim0=dim;
   dim=1;
end

[n,m]=size(array);

if dim == 1
   mn=zeros(1,m); 
   stdev=zeros(1,m);
   for ii = 1:m
      idx=find(~isnan(array(:,ii)));
      if length(idx) < 2
         if length(idx)==1
            mn(ii)=array(idx,ii);
         else
            mn(ii)=NaN;
         end
         stdev(ii)=NaN;
      else
         mn(ii)=mean(array(idx,ii));
         if nargout == 2
	    stdev(ii)=std(array(idx,ii));
         end
      end
   end
%	Reshape the result if necessary
   if exist('dim0','var')
      dims1=circshift(dims',1-dim0)';
      dims1(1)=1;
      mn=reshape(mn,dims1);
      mn=shiftdim(mn,ldims-dim0+1);
      stdev=reshape(stdev,dims1);
      stdev=shiftdim(mn,ldims-dim0+1);
   end

else
   mn=zeros(n,1,1); 
   stdev=zeros(n,1,1);
   for ii = 1:n
      idx=find(~isnan(array(ii,:)));
      if length(idx) < 2
         if length(idx)==1
            mn(ii)=array(ii,idx);
         else
            mn(ii)=NaN;
         end
         stdev(ii)=NaN;
      else
         mn(ii)=mean(array(ii,idx));
         if nargout == 2
	    stdev(ii)=std(array(ii,idx));
         end
      end
   end
end
