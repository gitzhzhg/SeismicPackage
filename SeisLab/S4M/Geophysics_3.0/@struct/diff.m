function ds=diff(ds,n,dim)
% Function computes the sample-to-sample difference of the traces of a seismic dataset
% See help for Matlab function "diff". Start and/or end times are changed in accordance
% with the setting of "S4M.diff_location".
%
% Written by: E. Rietsch: November 4, 2006
% Last updated:
%
%        ds=diff(ds)

global S4M

if nargin <= 2
   dim=1;
end
if nargin == 1
   n=1;
end

if isstruct(ds)  &&  strcmp(ds(1).type,'seismic')
   for ii=1:numel(ds)
      ds(ii).traces=diff(ds(ii).traces,n,dim);
   end

   switch S4M.diff_location
   case 'base'
      ds(ii).last=ds(ii).last-n*ds(ii).step;
   case 'top'
      ds(ii).first=ds(ii).first+n*ds(ii).step;
   case  'center'
      ds(ii).first=ds(ii).first+0.5*n*ds(ii).step;
      ds(ii).last=ds(ii).last-0.5*n*ds(ii).step;
   otherwise
      error(['Unknown option "S4M.diff_location": ',S4M.diff_location])
   end

else
   error('Function "diff" is not defined for this argument class.')
end
