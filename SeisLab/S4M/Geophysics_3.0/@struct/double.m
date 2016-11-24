function ds=double(ds)
% Function converts numeric fields of specific structures (seismic, well log, 
% table, pseudo-wells) from single precision to double precision; it does nothing
% if these fields are already in double precision.
%
% Written by: E. Rietsch: October 24, 2006
% Last updated: February 11, 2008: Handle vectors of structures
%
%       ds=double(ds)
% INPUT
% ds    structure with datasets of type "seismic", "well_log", ...
% OUTPUT
% ds    input structure with numeric fields converted to double precision

% UPDATE HISTORY
%      October 26, 2006: Extend to other types of datasets
%      January 17, 2007: Use "structfun" to streamline and speed-up the code


if strcmp(ds(1).type,'seismic')	 ||  strcmp(ds(1).type,'well_log') ...
                                  ||  strcmp(ds(1).type,'table') ...
                                  ||  strcmp(ds(1).type,'pseudo-wells') 
   fnames=fieldnames(ds);
   for jj=1:length(ds)
%       Modify all fields of the structure that are numeric and single-precision
      for ii=find(structfun(@(x) isnumeric(x) && isa(x,'single'),ds(jj)))'
         ds(jj).(fnames{ii})=double(ds(jj).(fnames{ii}));
      end
   end
else
   error('Operator "double" is not defined for this type of argument.')
end
