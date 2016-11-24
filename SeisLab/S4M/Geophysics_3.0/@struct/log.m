function ds=log(ds)
% Function takes the natural logarithm of the traces of seismic datasets
% or the non-depth curves of well logs.
%
% Written by: E. Rietsch: November 23, 2005
% Last updated: January 29, 2008: Handle well logs 

% UPDATE HISTORY
%       September 18, 2006: Handle structure arrays

if isstruct(ds)  
   if strcmp(ds(1).type,'seismic')
      for ii=1:numel(ds)
         ds(ii).traces=log(ds(ii).traces);
      end
   elseif strcmp(ds(1).type,'well_log')
      for ii=1:numel(ds)
         ds(ii).curves(:,2:end)=log(ds(ii).curves(:,2:end));
      end
   end
   
else
   error('Operator "log" is not defined for this argument.')
end
