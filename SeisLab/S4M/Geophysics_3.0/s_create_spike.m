function wavelet=s_create_spike(varargin)
% Create a spike (seismic dataset with a spike of default-amplitude 1 at a 
% specified time.
%
% See also: s_create_wavelet
%
% Written by: E. Rietsch: November 7, 2006
% Last updated: January 1, 2007: Add "null" field and convert to requested precision
%
%            wavelet=s_create_spike(varargin)
% INPUT
% varargin   one or more cell arrays; the first element of each cell array
%            is a keyword, the other elements are parameters.
%            Presently, keywords are:
%     'amplitude'   amplitude of spike
%            Default: {'amplitude',1}
%     'location'    time of spike
%            Default: {'location',0}
%     'step'  sample interval of spike
%            Default: {'step',4}
%     'times' start time and end time of the spike data set; can be two numbers
%            or a two-element vector
%            Default: {'times',0,0}
%
% EXAMPLE
%            spike=s_create_spike({'times',-100,100});
%            lfigure
%            subplot(1,2,1)
%               s_wplot(spike',{'interpol','linear'},{'figure','old'}, ...
%                              {'title','Wiggle representation'})
%            subplot(1,2,2)
%               s_wplot(spike',{'quality','spikes'},{'wiggle_color','red'}, ...
%                              {'figure','old'},{'title','Spike representation'})


run_presets_if_needed

%	Defaults of input parameter
param.amplitude=1;
param.times=[0,0];
param.location=0;
param.step=4;

%	Replace defaults by actual input arguments
param=assign_input(param,varargin);

if iscell(param.times)
   param.times=cell2num(param.times);
end
param.times(1)=floor(param.times(1)/param.step)*param.step;
param.times(2)= ceil(param.times(2)/param.step)*param.step;


if param.location < param.times(1)  ||  param.location > param.times(2)
   error('Spike location must not be outside of the time range specified for the seimic trace.')
end

nsamp=(param.times(2)-param.times(1))/param.step+1;
temp=zeros(nsamp,1);
temp(round((param.location-param.times(1))/param.step)+1)=param.amplitude;
wavelet=s_convert(temp,param.times(1),param.step);
wavelet.name='Spike';
