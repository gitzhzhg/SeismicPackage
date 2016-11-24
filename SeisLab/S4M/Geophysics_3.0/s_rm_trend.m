function [seisout,trend]=s_rm_trend(seismic,varargin)
% Function removes means (bias), medians, trends, etc. from a seismic data set
%
% Written by: E. Rietsch: July 2001
% Last updated: July 22, 2001: bug fix
%
%           seismic=s_rm_trend(seismic,varargin)
% INPUT
% seismic   seismic data set
% varargin  One or more cell arrays; the first element of each cell array is a
%           keyword, the other elements are parameters. Presently, keywords are:
%           'type'   type of function to remove from seismic traces. Possible
%                values are:
%                'mean'          remove mean of traces
%                'median'        remove median of traces
%                'median_slope'  remove median trend of traces
%                'median_line'   remove median linear function
%                Default: {'type','median'}
%           'option'  specifies if the above operation should be performed
%                for each trace separately ('trace') or for the 
%                data set as a whole ('dataset').
%                Default: {'option','trace'}
% OUTPUT
% seisout     seismic after removal operation
% trend       seismic structure with same headers, etc., as seisout but traces
%             represent the trend removed

global S4M

%  	Set defaults
param.type='mean';
param.option='trace';

%       Decode and input arguments and assign to defaults structure
param=assign_input(param,varargin);

[nsamp,ntr]=size(seismic.traces);
seisout=seismic;
if nargout == 2
   trend=seismic;
end

switch param.type

     	case 'median'
if strcmpi(param.option,'trace')
   med=median(seismic.traces);
   for ii=1:ntr
      seisout.traces(:,ii)=seisout.traces(:,ii)-med(ii);
   end
   if nargout == 2
      for ii=1:ntr
         trend.traces(:,ii)=med(ii);
      end
   end

elseif strcmpi(param.option,'dataset')
   med=median(seismic.traces(:));
   seisout.traces=seisout.traces-med;
   if nargout == 2
      trend.traces=med;
   end
else
   error([' Unknown option "',param.option,'"'])
end

     	case 'median_slope'
nsamp2=nsamp/2-0.5;
fact=(-nsamp2:1:nsamp2)'+eps;
if strcmpi(param.option,'trace')
   if mod(nsamp,2) == 1
      fact((nsamp+1)/2) = 1;
   end
   if nargout == 1
      trend.traces=zeros(nsamp,ntr);
   end
   for ii=1:ntr
      trend.traces(:,ii)=median(seisout.traces(:,ii)./fact)*fact;
   end
   seisout.traces=seisout.traces-trend.traces;

elseif strcmpi(param.option,'dataset')
   med=median(mean(seisout.traces,2)./fact);
   med=med*fact;
   med=med(:,ones(1,ntr));
   seisout.traces=seisout.traces-med;
   if nargout == 2
      trend.traces=med;
   end
else
   error([' Unknown option "',param.option,'"'])
end

     	case 'mean'
if strcmpi(param.option,'trace')
   med=mean(seismic.traces);
   for ii=1:ntr
      seisout.traces(:,ii)=seisout.traces(:,ii)-med(ii);
   end
   if nargout == 2
      for ii=1:ntr
         trend.traces(:,ii)=med(:,ii);
      end
   end

elseif strcmpi(param.option,'dataset')
   med=mean(seismic.traces(:));
   seisout.traces=seisout.traces-med;
   if nargout == 2
      trend.traces=med;
   end
else
   error([' Unknown option "',param.option,'"'])
end

       case 'median_line'
ti=(1:nsamp)';
alpha=0.1;
nd=nsamp;
if strcmpi(param.option,'trace')
   if nargout == 1
      trend.traces=zeros(nsamp,ntr);
   end
   for ii=1:ntr
      [a,b]=linfit1(ti,seismic.traces(:,ii),alpha,nd);
      trend.traces(:,ii)=a+b*ti;
      seisout.traces(:,ii)=seisout.traces(:,ii)-trend.traces(:,ii);
   end

elseif strcmpi(param.option,'dataset')
   temp=sum(seismic.traces,2);
   [a,b]=linfit1(ti,temp,alpha,nd);
   drift=a+b*ti;
   for ii=1:ntr
      seisout.traces(:,ii)=seisout.traces(:,ii)-drift;
   end
   if nargout == 2
      for ii=1:ntr
         trend.traces(:,ii)=drift;
      end
   end
else
   error([' Unknown option "',param.option,'"'])
end

		otherwise
error([' Unknown type of operation (',param.type,')'])

end		% End of switch block

htext='';
if S4M.history && isfield(seismic,'history')
   seisout=s_history(seisout,'append',htext);
   if nargout == 2
     trend=s_history(trend,'append','Trend');
   end
end
