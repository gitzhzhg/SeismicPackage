function seismic1=s_one2multi_trace(seismic)
% Function combines the traces of a dataset vector set into one multi-trace data set.
% The result is a structure vector of data sets
%
%           OBSOLETE!, please use "s_dsvector2ds" instead.
%
% Written by: E. Rietsch: October 2, 2005
% Last updated:
%
%           seismic1=s_one2multi_trace(seismic)
% INPUT
% seismic   seismic dataset vector
% OUTPUT
% seismic1  multi-trace seismic dataset;

alert('OBSOLETE!, please use "s_dsvector2ds" instead.')

seismic1=seismic(1);

for ii=2:size(seismic(:));
   seismic1=s_append(seismic1,seismic(ii));
end
