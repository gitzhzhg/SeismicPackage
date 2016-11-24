function wlog=l_average(wlog,varargin)
% Function computes running average over specified log curves
% The function needs equidistantly sampled data; no null values
%
% Written by: E. Rietsch, June 7, 2001
% Last updated: April 22, 2008: Handel vectors of logs.
%
%        wlog=l_average(wlog,varargin)
% INPUT
% wlog   log structure
% varargin one or more cell arrays; the first element of each cell array is 
%        a keyword, the following arguments are parameters. 
%        Accepted keywords are:         
%    'curves'   mnemonics of curves to average
%        Default: {'curves','*'}   meaning all curves
%    'length'   length of averaging filter and units of measurement
%        Default: {'length',wlog.step,wlog.curve_info{1,2}}   no averaging 
%                                                      (average over one sample)
%    'mnem' character string to append to mnemonic of original data
%        if "mnem" is empty (e.g. {'mnem',''}), then the original curves are
%        replaced by the averaged curves.
%        Default: {'mnem','mean'}, i.e. the string "mean" is appended to 
%                                  the mnemonics: Example DTp ==> DTp_mean.
%    'weight' type of smoothing; possible values are: 'constant' (weight is 
%        constant) and 'binom' (binomial weight).
%        Default: {'weight','constant')
%
% OUTPUT
% wlog   log structure with averaged curves
%
% EXAMPLE
%        wlog=l_data;
%        wlog=l_average(wlog,{'length',3.5},{'curves','vp'})
%        l_plot1(wlog,{'curves','vp','vp_mean'})

% UPDATE HISTORY
%        March 11, 2002. Remove pre-existing averaged curves before
%                               recreating them. Avoids having more than one 
%                               curve with the same mnemonic.


if ~istype(wlog,'well_log')
   error('First input argument must be a well log.')
end


%%       Set defaults of input arguments
param.curves='*';
param.length={wlog.step,wlog.curve_info{2,1}};
param.mnem='mean';
param.weight='constant';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

%     Analyze some input arguments
if ~iscell(param.length)
   param.length={param.length,wlog.curve_info{1,2}};
end
if ~iscell(param.curves)
   if strcmp(param.curves,'*')
      param.curves=wlog.curve_info(2:end,1);
   else
      param.curves={param.curves};
   end
end


%%       Handle well-log vectors
lwlog=length(wlog);
if lwlog > 1
   for ii=1:lwlog
      wlog(ii)=l_average(wlog(ii),{'curves',param.curves},{'length',param.length}, ...
         {'mnem',param.mnem},{'weight',param.weight});
   end
   return
end


if wlog.step == 0
   error(' Log must be uniformly sampled')
end

[nsamp,ncurves]=size(wlog.curves);

nnew=length(param.curves);

%%       Compute averaging distance in samples
dunits1=param.length{2};
dunits2=wlog.curve_info{1,2};
distance=param.length{1}/wlog.step;
if ~strcmp(dunits1,dunits2)
   if strcmp(dunits1,'m')  &&  strcmp(dunits2,'ft')
      distance=distance/0.3048;
   elseif strcmp(dunits1,'ft')  &&  strcmp(dunits2,'m')
      distance=distance*0.3048;
   elseif strcmp(dunits1,'s')  &&  strcmp(dunits2,'ms')
      distance=distance*1000;
   elseif strcmp(dunits1,'ms')  &&  strcmp(dunits2,'s')
      distance=distance/1000;
   else
      error([' Keyword "length" and depth curve have incompatible units:', ...
               dunits1,', ',dunits2])
   end
end

if isempty(param.mnem)
   for ii=1:nnew
      iloc=curve_index1(wlog,param.curves{ii});
      temp=wlog.curves(:,iloc);
      idx=find(~isnan(temp));
      ia=idx(1);
      ie=idx(end);
      wlog.curves(ia:ie,iloc)=mysmooth(temp(ia:ie),distance);
   end  

else
%     Check if curves already exist and delete those that do
   temss_curves=param.curves;
   for ii=1:length(param.curves)
      temss_curves{ii}=[temss_curves{ii},'_',param.mnem];
   end
   wlog=l_curve(wlog,'delete_ne',temss_curves);

%     Reserve space for new curves and curve information
   wlog.curves=[wlog.curves,NaN*zeros(nsamp,nnew)];
   curve_info=[wlog.curve_info;cell(nnew,3)];
   index=ncurves+(1:nnew);

   for ii=1:nnew
      iloc=curve_index1(wlog,param.curves{ii});
      temp=wlog.curves(:,iloc);
      idx=find(~isnan(temp));
      ia=idx(1);
      ie=idx(end);
      if strcmpi(param.weight,'constant')
         wlog.curves(ia:ie,index(ii))=mysmooth(temp(ia:ie),distance);
      elseif strcmpi(param.weight,'binom')
         wlog.curves(ia:ie,index(ii))=binsmooth(temp(ia:ie),distance);
      else
         error([' Unknown weight of smoothing: ',param.weight])
      end
      curve_info(index(ii),:)=[{[param.curves{ii},'_',param.mnem]}, ...
           wlog.curve_info(iloc,2),{[wlog.curve_info{iloc,3},' (averaged)']}];
   end
   wlog.curve_info=curve_info;
end
