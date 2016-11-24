function seismic1=s_ds2ds_vector(seismic,header)
% Convert data set into individual one-trace or multi-trace datasets 
% based on a header value. The result is a structure matrix of
% seismic data sets.
%
% OBSOLETE!  Replace by "s_ds2dsvector"
%
% See also: s_dsvector2ds
%
% Written by: E. Rietsch: September 18, 2006
% Last updated:
%
%           seismic1=s_ds2sd_vector(seismic,header)
% INPUT
% seismic   seismic dataset
% header    optional; mnemonic of a header; all traces with the same header 
%           value are collected in an entry of the data set vector.
%           That header value is added to the dataset name as (header = headervalue)
%           Default: header='trace_no'
% OUTPUT
% seismic1  structure vector of one-trace datasets; thus selecting the third
%           trace
%              temp=s_select(seismic,{'traces',3}), 
%	    where "seismic" is the input dataset, can now be achieved by
%              temp=seismic1(3)
%           where "seismic1" is the output data set
%
% EXAMPLE
%           seismic=s_data;
%           seismic1=s_ds2ds_vector(seismic)
%           s_compare(seismic1(2),s_select(seismic,{'traces',2})) % Must be the same

alert('OBSOLETE!  Replace by "s_ds2dsvector".')

if nargin == 1
   seismic1=s_ds2dsvector(seismic);
else
   seismic1=s_ds2dsvector(seismic,header);
end
