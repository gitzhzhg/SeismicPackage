function seismic=s_data
% Generate test dataset consisting of 12 traces of filtered random Gaussian
% noise (1000 ms long, 4 ms sample interval)
% The dataset has one header ('CDP')
%
% Written by: E. Rietsch: August 27, 2003
% Last updated: November 5, 2009: Read data from file; perform computation
%                                 only if file has not been found
% 
%           seismic=s_data

% UPDATE HISTORY
%        October 23, 2006: Filter data in CDP space

run_presets_if_needed
global S4M

%   If the file exists, red data from file
filename=fullfile(S4M.testdata,'data.mat');
if exist(filename,'file')
   load(filename)  % Dataset loaded has name "seismic"
   return
end

%   Since the file has not been found, create data
%        Disable history generation
history=S4M.history;
S4M.history=false;

randn('state',99999)
temp=randn(251,20);

%       Filter the "traces" in the space dimension to improve continuity 
ftemp=fft(temp,[],2);
ftemp(:,4:18)=0;
temp=ifft(ftemp,[],2);
temp(1:10,5:16)=0;

seismic=s_convert(temp(:,5:16),0,4);
seismic=s_filter(seismic,{'ormsby',10,15,30,60});
if strcmp(S4M.precision,'single')
   cdp=single(101:100+size(seismic.traces,2));
else
   cdp=double(101:100+size(seismic.traces,2));
end

seismic=ds_header(seismic,'add','CDP',cdp,'n/a','CDP number');
seismic.name='Test data';

%       Enable history-field generation (if requested)
S4M.history=history;
seismic=s_history(seismic,'add','Synthetic filtered noise');
