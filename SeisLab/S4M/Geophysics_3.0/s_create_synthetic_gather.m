function seismic=s_create_synthetic_gather(times,varargin)
% Compute a synthetic gather consisting of individual events (spikes) 
% with linear, parabolic, hyperbolic, or no moveout.
%
% Written by: E. Rietsch, June 22, 2001
% Last updated: February 10, 2008: bug fix
%
%          seismic=s_create_synthetic_gather(times,varargin)
% INPUT
% times      vector of equidistant times for which to compute the gather; 
%            units are ms.
% varargin   one or more cell arrays; the first element of each cell array
%            is a keyword,the other elements are parameters. Presently, 
%            keywords are (the first three listed have no defaults):
%     'offsets' Vector of offsets (either angles or offsets must be specified)
%            followed by string with units of measurement ('ft' or 'm'); 
%            The function performs unit conversion to make offsets and 
%            velocities compatible.
%            Default: {'offsets',[]}  (this means there is no default)
%     'angles' Vector of angles (either angles or offsets must be specified)
%            Default: {'angles',[]}  (this means there is no default)   
%     'times'  vector of times with zero-offset (zero-angle) reflections; 
%            units are ms.
%            Default: {'times',[]}  (this means there is no default)
%
%     'amplitudes' vector with amplitudes of these reflections; must have 
%            either as many elements as 'times' or be a scalar
%            Default: {'amplitudes',1}
%     'gradients'  vector with gradients 
%            amplitude(offset) = amplitude(0) + gradient * offset^2
%                       or
%            amplitude(angle) = amplitude(0) + gradient * sin(angle)^2
%            must have either as many elements as 'times' or be a scalar
%            Default: {'gradients',0}
%     'moveout' possible values are 'hyperbolic', 'linear', or 'no'
%            Default: {'moveout','hyperbolic'} 
%     'units' units of measurement of time axis 
%            Default: {'units','ms'}
%     'velocities' vector with velocities followed by units of measurement 
%            ('m/s' or 'ft/s'); if no units of measuremet are specified then
%            'm/s' is assumed.
%            must have either as many elements as 'times' or be a scalar
%            Default: {'velocities',1500,'m/s'}            
%            
% OUTPUT
% seismic    seismic structure
%
% EXAMPLE
%           spikes=s_create_synthetic_gather(0:4:700,{'times',[100,200,300]}, ...
%                   {'moveout','parabolic'},{'offsets',0:50:500,'m'}, ...
%                   {'amplitudes',[1 -1.5 2]},{'gradients',[0,1/500,2/500].^2});
%           seismic=s_filter(spikes,{'ormsby',5,15,30,60});
%           lfigure
%           subplot(1,2,1)
%           s_wplot(spikes,{'interpol','linear'},{'figure','old'})
%           subplot(1,2,2)
%           s_wplot(seismic,{'figure','old'})

% UPDATE HISTORY
%           October 238, 2005: Added fields "type", "tag", "name"
%           July 13, 2007: more consistency checking, double and single precision


global S4M

run_presets_if_needed

%	Defaults of input arguments
param.offsets=[];
param.angles=[];
param.times=[];
param.amplitudes=1;
param.gradients=0;
param.moveout='hyperbolic';
param.units='ms';
param.velocities={1500,'m/s'};

%       Decode and assign input arguments
param=assign_input(param,varargin);

% 	Initial consistency checks and parameter preparation
nt=length(param.times);
if iscell(param.times)
   param.times=cell2mat(param.times(:));
else
   param.times=param.times(:);
end


%       Check amplitudes specified
namp=length(param.amplitudes);
if iscell(param.amplitudes)
   param.amplitudes=cell2mat(param.amplitudes);
end
if nt ~= namp;
   if namp == 1
      ampls=param.amplitudes*ones(nt,1);
   else
      error([' Number of amplitudes specified must be 1 or ', ...
            'equal to number of reflection times'])
   end
else
   ampls=param.amplitudes(:);
end



%       Check gradients specified
if iscell(param.gradients)
   param.gradients=cell2mat(param.gradients(:));
end
namp=length(param.gradients);
if nt ~= namp;
   if namp == 1
      grads=param.gradients*ones(nt,1);
   else
      error([' Number of gradients specified must be 1 or ', ...
            'equal to number of reflection times'])
   end
else
   grads=param.gradients(:);
end


%       Check velocities specified
if iscell(param.velocities)
   if ischar(param.velocities{end})
      vunits=param.velocities{end};
      temp=param.velocities(1:end-1);
      len=length(temp);
      if len == 1
         vels=temp{1};
      elseif len > 1
         vels=cell2mat(temp);
      else
         error(' No velocities specified.')
      end
   else
      vunits='m/s';
   end
else
   vels=param.velocities;
   vunits='m/s';
end

namp=length(vels);
if nt ~= namp;
   if namp == 1
      vels=vels*ones(nt,1);
   else
      error([' Number of velocities specified must be 1 or ', ...
            'equal to number of reflection times'])
   end
else
   vels=vels(:);
end

%       Create fields of output data set
nsamp=length(times);
seismic.type='seismic';
seismic.tag='unspecified';
seismic.name='Synthetic gather';
seismic.first=times(1);
seismic.last=times(end);
seismic.step=(times(end)-times(1))/max(nsamp-1,eps);
seismic.units='ms';

if isempty(param.offsets)
   if isempty(param.angles)
      error(' Angles or offsets must be specified')
   end
   ntr=length(param.angles);
   seismic.header=reshape(param.angles,1,ntr);
   seismic.header_info={'angle','n/a','Angle of incidence'};

else
   if iscell(param.offsets)
      if ischar(param.offsets{end})
         lunits=param.offsets{end};
         temp=param.offsets(1:end-1);
         len=length(temp);
         if len == 1
            offsets=temp{1};
         elseif len > 1
            offsets=cell2mat(temp);
            offsets=offsets(:);
         else
            error(' No offsets specified')
         end
      else
         lunits='m';
      end
   else
      offsets=param.offsets;
      lunits='m';
   end
   ntr=length(offsets);
   seismic.headers=reshape(offsets,1,ntr);
   seismic.header_info={'offset',lunits,'Offset'};
end

%       Check consistency of units of measurement
if ~((strcmp(vunits,'m/s')  &&  strcmp(lunits,'m')) || ...
    (strcmp(vunits,'ft/s')  &&  strcmp(lunits,'ft')))
   error([' Inconsistent offset and velocity units: "',lunits,'", "',vunits,'"' ])
end

seismic.traces=zeros(nsamp,ntr,S4M.precision);

%       Select reflection times in time range
switch param.moveout
case {'hyperbolic','parabolic'}
   rtimes2=param.times(:).^2;
%   grads=grads/max(offsets(end)^2,eps);
   rvels2=1.0e6./vels.^2;

case 'no'
%   grads=grads/offsets(end);

case 'linear'
%   grads=grads/offsets(end);
   rvels=1000./vels;

otherwise
   error([' Unknown type of moveout: ',param.moveout])
end


for ii=1:ntr
   switch param.moveout
   case 'hyperbolic'
      o2=offsets(ii)^2;
      temp=sqrt(rtimes2+o2*rvels2);

   case 'parabolic'
      o2=0.5*offsets(ii)^2;
      temp=param.times(:)+o2*rvels2./param.times(:);

   case 'linear'
      o2=offsets(ii);
      temp=param.times(:)+offsets(ii)*rvels;
   
   case 'no'
      temp=param.times(:);
      o2=offsets(ii);
   
   otherwise
      error([' Unknown type of moveout: ',param.moveout])
   end

   index=find(temp >= seismic.first  &  temp <= seismic.last);
   if ~isempty(index)    % If there are events within the seismic time range
      temp=temp(index)-seismic.first; 
      atemp=ampls(index);
      gtemp=grads(index);
      temp1=temp/seismic.step+1;
      temp2=floor(temp1);
      temp3=temp2+1;
      atempgrad=atemp+o2*gtemp;
      atemp2=(temp3-temp1).*atempgrad;
      atemp3=(temp1-temp2).*atempgrad;
      if temp3(end) > nsamp
         temp3=temp3(1:end-1);
         atemp3=atemp3(1:end-1);
      end
      seismic.traces(temp2,ii)=seismic.traces(temp2,ii)+atemp2;
      if ~isempty(temp3)
         seismic.traces(temp3,ii)=seismic.traces(temp3,ii)+atemp3;
      end
   end
end

%	Check for NaNs
index=find(isnan(seismic.traces),1);
if ~isempty(index)
   seismic.null=NaN;
else
   seismic.null=[];
end

%       Determine precision
if strcmpi(S4M.precision,'single')
   seismic=single(seismic);
end

%       Add history field
seismic=s_history(seismic,'add',['Moveout: ',param.moveout]);
