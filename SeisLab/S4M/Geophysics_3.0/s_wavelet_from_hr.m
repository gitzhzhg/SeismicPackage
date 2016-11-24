function [seismic,header]=s_wavelet_from_hr(filename)  %#ok
% Read wavelet in Hampson-Russell format from ASCII file 
% Written by: E. Rietsch: October 18, 2005
% Last updated:
%
% OBSOLETE: replaced by "s_wavelet_from_hampson_russell"
%           [seismic,header]=s_wavelet_from_hr(filename)
% INPUT
% filename  file name (optional)
%           the filename and the directory are saved in global variable S4M
% OUTPUT
% seismic   seismic data set read from file
% header    text header of Hampson-Russell file

alert('OBSOLETE: use "s_wavelet_from_hampson_russell" instead.')
error('Abnormal termination.')
