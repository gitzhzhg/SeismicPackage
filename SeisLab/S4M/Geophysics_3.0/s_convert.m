function seismic=s_convert(traces,first,step,history,units)
% Function creates a minimal seismic data structure (the history field is optional)
%
% Written by; E. Rietsch
% Last updated: October 7, 2006: change output to requested precision
%
%            seismic=s_create(traces,first,step,history,units)
% INPUT
% traces     array of seismic traces
% first      start time/frequency/depth
% step       sample interval
% history    optional string to put into history field
% units      string with units of measurements (default: 'ms')
% OUTPUT
% seismic    seismic structure satisfying minimal requirements
%
% EXAMPLE
%            seismic=s_convert(randn(251,12),0,4,[],'ft');
%            s_wplot(seismic)

global S4M

run_presets_if_needed

%	Define standard fields of a seismic dataset
seismic.type='seismic';
seismic.tag='unspecified';
seismic.name='';
seismic.first=first;
seismic.last=first+(size(traces,1)-1)*step;
seismic.step=step;

if nargin < 5
   seismic.units='ms';
else
   seismic.units=units;
end

seismic.traces=traces;

if any(isnan(traces(:)))   % Check for NaNs
   seismic.null=NaN;
else
   seismic.null=[];
end

%	Convert to the requested precision
if strcmpi(S4M.precision,'single')
   seismic=single(seismic);
else
   seismic=double(seismic);
end


%       Add a history field to the seismic dataset
if S4M.history
   if nargin < 4
      history='';
   end
   seismic=s_history(seismic,'add',history);
end
