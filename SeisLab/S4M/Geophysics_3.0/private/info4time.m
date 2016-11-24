function info=info4time(seismic)
% Create a standard cell row vector with info about the time axis
% of the seismic input dataset.
%
% Written by: E. Rietsch: September 14, 2003
% Last updated: September 19, 2009: Add default for yet undefined units
%
%          info=info4time(seismic)
% INPUT
% seismic  seismic data set
% OUTPUT
% info     three-element cell row vector with mnemonic, units of measurement,
%          and description for the time axis
%
% EXAMPLE
%          info4time(s_fft(s_data))

% UPDATE HISTORY
%          July 28, 2006: speed-up of selection
%          January 18, 2009: Add option of "rows' for units

info=cell(1,3);
info{2}=seismic.units;

temp1={'time','time','time','time','frequency','frequency','depth','depth'};
temp2={'Time','Time','Time','Time','Frequency','Frequency','Depth','Depth'};

idx=find(ismember({'ms','msec','s','sec','hz','khz','ft','m'},lower(seismic.units)));

if ~isempty(idx)
   info{1}=temp1{idx};
   info{3}=temp2{idx};

elseif strcmpi('samples',seismic.units)
   info{1}='sample';
   info{2}='n/a';
   info{3}='Samples';
   
elseif strcmpi('rows',seismic.units)
   info{1}='rows';
   info{2}='n/a';
   info{3}='Rows';
   
elseif strcmpi('histogram',seismic.tag)
   if strcmpi(seismic.binsize,'equal')
      info{1}='amplitude';
      info{2}='';
      info{3}='Binned amplitude';
   else
      info{1}='binindex';
      info{2}='unequal bins';
      info{3}='Bin index';
   end
 
else
   info{1}=info{2};
   info{2}='n/a';
   info{3}=info{1};
   
end 
