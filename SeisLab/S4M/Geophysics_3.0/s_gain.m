function [gseismic,tgain]=s_gain(seismic,varargin)
% Function computes and applies a gain function to the input data set and
% outputs the gained data set as well as the gain function (if two output 
% data sets are specified)
%
% Written by: E. Rietsch: July 14, 2000
% Last updated: October 2, 2001: Handle NaNs in data (at least in some cases}
%
%             [gseismic,tgain]=s_gain(seismic,varargin)
% INPUT
% seismic     seismic data structure
% varagin     one or more cell arrays; the first element of each cell array is 
%             a keyword, the other elements are parameters. 
%             Presently, keywords are:
%        'type' Type of gain. Possible values are 'trace' (each trace is 
%             gained individually) and 'dataset' (one gain for the whole data set).
%             Default: {type','trace'}
%        'average' Type of average to use when one gain is used for the whole dataset.
%             Possible values are: 'mean' (i.e. mean of absolute values) and 
%                                  'median' (i.e. median of absolute values).
%             Default: {average','median'}
%        'wlength'  Window length (ms) for automatic gain calculation.
%             Default: {'wlength',500)
% OUTPUT
% gseismic    seismic data structure with gained input data set
% tgain       gain applied to input seismic data set.

if ~isstruct(seismic)
   error(' First input argument must be seismic structure')
end
[nsamp,ntr]=size(seismic.traces);

%       Set default values
param.average='median';
param.type='trace';
param.window=[];
param.wlength=500;

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

% 
if ~isempty(param.window) 
   param.wlength=param.window;
   alert('Parameter "window" is obsolete; replace it with "wlength".')
end

%       Set window length in samples
wlength=fix(param.wlength/seismic.step)+1;
if wlength > nsamp
   wlength=nsamp;
end
if ~mod(wlength,2)
   wlength=max(wlength-1,1);
end

if isnull(seismic)
   if 1
      error(' NaNs in seismic dataset not yet allowed.')
   end

   if strcmpi(param.type,'trace')
      tgain_traces=trace_gain(seismic.traces,wlength);
      gseismic.traces=seismic.traces./tgain_traces;
      htext='trace gain';
   elseif strcmpi(param.type,'dataset')
      tgain_traces=trace_gain4nans(seismic.traces,wlength);
      gseismic.traces=seismic.traces./tgain_traces;
      htext='suite gain';
   else
      error('Unknown gain type')
   end
else
   if strcmpi(param.type,'trace')
      tgain_traces=trace_gain(seismic.traces,wlength);
      gseismic.traces=seismic.traces./tgain_traces;
      htext='trace gain';

   elseif strcmpi(param.type,'dataset')
%     nsamp=size(seismic.traces,1);
      tgain_traces=suite_gain(seismic.traces,wlength,param.average);
      for ii=1:ntr
         gseismic.traces(:,ii)=seismic.traces(:,ii)./tgain_traces;
      end
      htext='suite gain';

   else
      error('Unknown gain type')
  end
end
%       Copy rest of fields
gseismic=copy_fields(seismic,gseismic);

%    Append history field
htext=[htext,', window length = ',num2str(param.wlength),' ',seismic.units];
gseismic=s_history(gseismic,'append',htext);

%       Convert tgain to seismic structure
if nargout > 1
   tgain=s_convert(tgain_traces,seismic.first,seismic.step, ...
         seismic.units);
   tgain=s_history(tgain,'add',htext);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function gain=suite_gain(in,wl,average)
% Function computes time-varying gain over window of wl samples length for 
% the whole dataset
%        gain=tv_gain(in,wl,average)
% INPUT
% in     	input traces
% wl     	window length in samples (should be odd);
% average       type of averaging ('mean' or 'median')
% OUTPUT
% gain          gain function

wlh=floor(wl/2);
if strcmpi(average,'mean')
  amps=mean(abs(in),2);
else
  amps=median(abs(in),2);
end
amps=[0;cumsum(amps)];
gf=amps(wl+1:end)-amps(1:end-wl);
ga=gf(1);
ge=gf(end);
gain=[ga(ones(wlh,1),:);gf;ge(ones(wlh,1))]+eps;
gain=gain/mean(gain);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function gain=trace_gain(in,wl)
% Function computes, for each trace, time-varying gain over window of wl samples length
%           gain=trace_gain(in,wl)
% INPUT
% in     	input traces
% wl     	window length in samples (should be odd);
% OUTPUT
% gain         gain function

m=size(in,2);
wlh=floor(wl/2);
amps=[zeros(1,m);cumsum(abs(in))];
gf=amps(wl+1:end,:)-amps(1:end-wl,:);
ga=gf(1,:);
ge=gf(end,:);
gain=[ga(ones(wlh,1),:);gf;ge(ones(wlh,1),:)]+eps;
mgain=mean(gain(:));
for ii=1:m
   gain(:,ii)=gain(:,ii)/mgain;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function gain=trace_gain4nans(in,wl)
% Function computes, for each trace, time-varying gain over window of wl samples length
%           gain=trace_gain(in,wl)
% INPUT
% in     	input traces
% wl     	window length in samples (should be odd);
% OUTPUT
% gain          gain function

wlh=floor(wl/2);
[n,m]=size(in);

scf=ones(n+1,m);

lidx=isnan(in);
in(lidx)=0;
scf(lidx)=0;
scf=cumsum(scf);
amps=[zeros(1,m);cumsum(abs(in))];
gf=(amps(wl+1:end,:)-amps(1:end-wl,:))./(scf(wl+1:end,:)-scf(1:end-wl,:)+eps);
ga=gf(1,:);
ge=gf(end,:);
gain=[ga(ones(wlh,1),:);gf;ge(ones(wlh,1),:)]+eps;
mgain=mean(gain(:));
for ii=1:m
   gain(:,ii)=gain(:,ii)/mgain;
end
