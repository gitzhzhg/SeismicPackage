function [values,times,index]=s_get_extrema(seismic,option)
% Determine, for each trace, the value of the requested trace extremum and 
% the time and the sample index where they occur for the first time.
%
% Written by: E. Rietsch: May 8, 2006
% Last updated: 
%
%          [values,times,index]=s_get_extrema(seismic,option)
% INPUT
% seismic  seismic dataset
% option   string indicating typ of extremum; possible values are 'max','min'
% OUTPUT
% values   row vector with the values of the extrema (one for each trace)
% times    row vector with the times where the first extreme value occurs on 
%          each trace
% index    row vector of sample indices of the extreme values

switch option
case 'max'
   [values,index]=max(seismic.traces);

case 'min'
   [values,index]=min(seismic.traces);

otherwise
   error('Unknown option.')

end

times=(index-1)*seismic.step+seismic.first;
