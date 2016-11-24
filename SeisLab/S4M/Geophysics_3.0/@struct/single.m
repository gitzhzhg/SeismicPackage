function ds=single(ds)
% Function converts numeric fields of specific structures (seismic, well log, 
% table, pseudo-wells) from double precision to single precision; it does nothing
% if these fields are already in single precision. 
%
% Written by: E. Rietsch: October 24, 2006
% Last updated: February 11, 2008: Handle vectors of structures
% 
%        ds=single(ds)
% INPUT
% ds     structure with datasets of type "seismic", "well_log", ...
% OUTPUT
% ds     input structure with numeric fields converted to single precision

% UPDATE HISTORY
%      October 26, 2006: Extend to other types of datasets
%      January 17, 2007: Use "structfun" to streamline and speed-up the code


if strcmp(ds(1).type,'seismic')	 ||  strcmp(ds(1).type,'well_log') ...
                                  ||  strcmp(ds(1).type,'table') ...
                                  ||  strcmp(ds(1).type,'pseudo-wells') 
   fnames=fieldnames(ds);

%       Modify all fields of the structure that are numeric and double-precision
   for jj=1:length(ds)
      for ii=find(structfun(@(x) isnumeric(x) && isa(x,'double'),ds(jj)))'
         ds(jj).(fnames{ii})=single(ds(jj).(fnames{ii}));
      end
   end
else
   error('Operator "single" is not defined for the type of this argument.')
end
