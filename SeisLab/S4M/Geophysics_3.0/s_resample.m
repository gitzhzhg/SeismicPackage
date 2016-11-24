function seismic=s_resample(seismic,sample_interval,varargin)
% Function resamples seismic data to new sample interval. If the new sample
% interval is greater than the old sample interval and interpolation is done 
% in the time domain an Ormsby filter with corner frequencies 
%       0, 0, 0.8*fnyquist, fnyquist
% is applied to the data prior to resampling. 
% "fnyquist" is the Nyquist frequency associated with the new sample interval.
%
% Written by: E. Rietsch: April 14, 2000
% Last update: January 1, 2007: Assure that the precision of the input datset
%                               is the same as the output dataset.
%
%            seismic=s_resample(seismic,sample_interval,varargin)
% INPUT
% seismic    seismic structure
% sample_interval new sample interval (can be larger or smaller than seismic.step);
% varargin   one or more cell arrays; the first element of each cell array 
%            is a keyword, the other elements are parameters. 
%            Presently, keywords are:
%     'option'   parameter which specifies the kind of interpolation. 
%            Possible values are:
%            'standard' Straight forward interpolation. Frequency-domain anti-alias filter.
%                if sample_interval > seismic.step.
%            'smooth'   Straightforward interpolation. Time-domain smoothing if 
%                sample_interval > seismic.step
%            'wavelet'  Interpolation intended for wavelets. This interpolation  
%                includes the sample prior to the first and the one after the 
%                last in the interpolation, assuming they are zero.
%             Default: {'option','standard'}
%     'domain'   parameter specifies it interpolation is to be done in the frequency 
%            domain ('frequency') or in the time domain ('time').
%            Default: {'domain','time')
%     'filter'   parameter specifies if band-pass filter is to be applied 
%            (to the input data  if seismic.step < sample_interval (anti-alias)  
%            to the output data if seismic.step > sample_interval)
%            Default: {'filter','yes'}
% OUTPUT
% seismic     seismic structure after resampling
%
% EXAMPLE
%           wavelet4=s_create_wavelet;
%           wavelet4.name='4-ms wavelet';
%           wavelet2=s_resample(wavelet4,2);
%           wavelet2.name='2-ms wavelet';
%           s_compare(wavelet4,wavelet2,{'interpol','linear'},{'times',-40,40})

global S4M

%%       Set default values for input arguments
param.domain='time';
param.filter='yes';
param.option='standard';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);


%%  Handle case where "seismic" is a dataset vector
nseismic=length(seismic);
if nseismic > 1
   for ii=1:nseismic
      seismic(ii)=s_resample(seismic(ii),sample_interval,{'domain',param.domain}, ...
             {'filter',param.filter},{'option',param.option});
   end
   return
end


%%       Do nothing if sample interval is not changed
if seismic.step == sample_interval
   return
end

%	Change the sample interval if seismic is a spike (start and end time are the same)
if seismic.first == seismic.last
   if strcmp(class(seismic.traces),'single')
      seismic.step=single(sample_interval);
   else
      seismic.step=double(sample_interval);
   end
   return
end


ntr=size(seismic.traces,2);

%     Remove trace nulls
seismic=s_rm_trace_nulls(seismic);

first=floor(seismic.first/sample_interval)*sample_interval;
last=ceil(seismic.last/sample_interval)*sample_interval;
seismic=s_select(seismic,{'times',first,last});
new_times=(first:sample_interval:seismic.last)';
nsamp=length(new_times);

%%
switch param.option

               case 'standard'

if isnull(seismic)
   error(' Handling of null values not yet implemented')
end  

if seismic.step > sample_interval
   seismic.traces=interpolate(seismic.first:seismic.step:seismic.last,seismic.traces,new_times, ...
          param.domain,param.filter);

else
   seismic.traces=interpolate(seismic.first:seismic.step:seismic.last,seismic.traces,new_times, ...
          param.domain,param.filter);
end


               case 'smooth'

if isnull(seismic)
   error(' Handling of null values not yet implemented')
end  

if seismic.step > sample_interval
%   seismic.traces=interp1(seismic.first:seismic.step:seismic.last,seismic.traces,new_times,param.domain);
   seismic.traces=interpolate(seismic.first:seismic.step:seismic.last,seismic.traces,new_times, ...
          param.domain,param.filter);

else
   ratio=sample_interval/seismic.step;
   times=(seismic.first:seismic.step:seismic.last)';
   for ii=1:ntr
      temp=seismic.traces(:,ii);
      idx=find(~isnan(temp));
      temp(idx)=mysmooth(temp(idx),ratio);  
      seismic.traces(1:nsamp,ii)=interpolate(times,temp,new_times, ...
             param.domain,param.filter);
   end
   seismic.traces=seismic.traces(1:nsamp,:);

end

              case 'wavelet'

if seismic.step > sample_interval
   seismic.traces=interpolate((seismic.first-seismic.step:seismic.step:seismic.last+seismic.step)', ...
      [zeros(1,ntr);seismic.traces;zeros(1,ntr)],new_times,param.domain,param.filter);
else
   fnyquist=500/sample_interval;
   temp=ormsby([zeros(1,ntr);seismic.traces;zeros(1,ntr)],seismic.step,0,0,0.8*fnyquist,fnyquist);
   seismic.traces=interpolate((seismic.first-seismic.step:seismic.step:seismic.last+seismic.step)', ...
       temp,new_times,param.domain,param.filter);
   if any(isnan(seismic.traces))
      seismic.null=NaN;
      temp=S4M.history;            
      S4M.history=false;        % Make no entry in "history" field
      seismic=s_rm_trace_nulls(seismic);
      S4M.history=temp;
   end
end

           otherwise

error([' Unknown RESAMPLE option "',param.option,'"'])

end         % End of switch block


seismic.first=first;
seismic.last=new_times(end);
seismic.step=sample_interval;


%       Compatibility test (for frequency-domain interpolation)
if strcmpi(param.domain,'frequency')
   if size(seismic.traces,1) ~= nsamp
      try
         seismic.traces=seismic.traces(1:nsamp,:);
      catch
         keyboard
      end
   end
end


%       Check for NaNs
if any(isnan(seismic.traces(:)))
   seismic.null=NaN;
end

%       Append history field
if isfield(seismic,'history') && S4M.history
   htext=['to ',num2str(sample_interval),' ',seismic.units, ...
         ' (',param.option,', ',param.domain,' domain)'];
   seismic=s_history(seismic,'append',htext);
end 

%	Assure that the precision orf the output dataset is that of the input data set
if strcmp(class(seismic.traces),'single')
   seismic=single(seismic);
end


%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function ynew=interpolate(xold,yold,xnew,type,filter)
% Function performs interpolation in time or frequency domain, 
% assumes "xold" and "xnew" are uniform
% "type" is either 'time' or 'frequency';
% "filter" is either 'yes' or 'no' (only used if "type" is 'time')


dxold=mean(diff(xold));
dxnew=mean(diff(xnew));

if strcmpi(type,'time')
  
   if (dxold < dxnew) && strcmpi(filter,'yes')
      fnyquist=500/dxnew;
      yold=ormsby(yold,dxold,0,0,0.8*fnyquist,fnyquist);
   end
   ynew=interp1(xold,yold,xnew,'*cubic');
   if (dxold > dxnew) && strcmpi(filter,'yes')
      fnyquist=500/dxold;
      ynew=ormsby(ynew,dxold,0,0,fnyquist,1.2*fnyquist);
   end

elseif strcmpi(type,'frequency')
 
  if dxnew > dxold
     [nsamp,ntr]=size(yold);
     ratio=round(dxnew/dxold);
     lold=ratio*length(xnew);
     if lold > nsamp
        yold=[yold;zeros(lold-length(xold),ntr)];
     end
  end
 
  ynew=interpf(yold,dxold,dxnew);

else
   error([' Unknown domain for resampling: ',type'])
end
