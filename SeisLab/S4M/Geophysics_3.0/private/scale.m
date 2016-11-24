function [scaled,scale]=scale(a,b,type)
% Function scales columns of a so that they best match b.
% a and b must have the same number of columns.
% type indicates if one scale for all column is to be computed (type=1)
% or if each column should be scaled separately (any other value)
%
%	[scaled,scale]=scale(a,b,type)

ma=size(a,2); 
mb=size(b,2);
if ma ~= mb, 
   fprintf('ERROR in scale: input arrays have different number of columns (%d, %d)\n',ma,mb)
   return
end

cc=correlate(a,b); 
mcc=max(cc);
ac=sum(a.*a);
if type == 1,
   scale=sum(mcc,2)/sum(ac,2);
   scaled=scale*a;
else
   scale=mcc./ac;
   scaled=zeros(size(a));
   for ii=1:size(a,2);
      scaled(:,ii)=a(:,ii)*scale(ii);
   end
end
