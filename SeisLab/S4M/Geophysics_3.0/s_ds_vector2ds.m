function seismic1=s_ds_vector2ds(seismic)
% Combine the traces of a dataset vector into a single seismic dataset.
% OBSOLETE!  Replace by "s_dsvector2ds"
%
% See also: s_ds2dsvector
%      
% Written by: E. Rietsch: September 19, 2006
% Last updated: May 2, 2007: Improved handling of new dataset name.
%
%           seismic1=s_ds_vector2ds(seismic)
% INPUT
% seismic   seismic dataset vector
% OUTPUT
% seismic1  single seismic dataset; 
%           the field "name" of the output dataset is set to "seismic(1).name"
%           provided it is the same for all datasets;
%           if not, is set to the variable name of the input dataset vector, 
%           if it is not empty, or to "Seismic from DS vector" if it is empty.
%
% EXAMPLE
%           seismic=s_data;
%           seismic1=s_ds2ds_vector(seismic);   % Create dataset vector
%           seismic2=s_ds_vector2ds(seismic1);  % Convert dataset vector back to single dataset
%           s_compare(seismic,seismic2)         % Must be the same

alert('OBSOLETE!  Replace by "s_dsvector2ds".')

seismic1=s_dsvector2ds(seismic);
