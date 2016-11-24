function [corr,aux]=s_correlate(seis1,seis2,varargin)
% Function computes the crosscorrelation of two seismic data sets
% Traces of the second data set are shifted with respect to the first data set
% Correlations where time t1 of the first data set is aligned with time t2 of the
% second data set are associated with time t2-t1 of the output data set.
% Correlations can be performed in two ways:
%         1. Each trace of "seis2" is correlated with each trace of "seis1"
%         2. Each trace of "seis2" is correlated with the corresponding trace of "seis1";
%            in this case "seis1" and "seis2" must have the same number of traces.
% 
% Written by:  E. Rietsch: November 21, 2000
% Last updated: August 14, 2006: Add second output argument
%
%          corr=s_correlate(seis1,seis2,varargin)
% INPUT
% seis1    first seismic dataset   
% seis2    second seismic dataset
%          the two seismic datasets must have the same sample interval and
%          the difference in  start time must be an integer multiple of the 
%          this sample interval.
% varargin   one or more cell arrays; the first element of each cell array
%          is a keyword, the other elements are parameters. 
%          Presently, keywords are:
%    'lags'  two numbers representing the first and last shift of seis2 with
%          respect to seis1 for which the correlation is to be computed. 
%          They become the times of the first and last sample, respectively, 
%          of the output data set and must be in integer multiple of the sample 
%          interval of the seismic input data sets (they are changed to an
%          integer multiple of the sample interval if this is not the case).
%          Default: {'times',min([t1,t2]),max([t1,t2])}    
%                                     where  t1 = seis1.first - seis2.first;
%                                     t2 = t1+abs(seis1.last - seis2.last - t1);
%    'normalize'  Possible values are:    
%          'no'      ==> no normalization
%          'traces'  ==> traces of "seis1" and "seis2" are normalized to have 
%                L2 norm 1
%          'segment' ==> trace segments correlated are normalized individually;
%                in this case the lags must be within the range defined by
%                the default.                     
%          Default: {'normalize','traces'}  
%   
%    'option'  describes the type of operation performed. Possible values are: 
%          'all' which performs correlates every trace of "seis1" with every 
%                trace of "seis2". The output traces represent "seis1" correlated
%                with the first trace of "seis2" followed by "seis1" correlated
%                with the second trace of "seis2", etc.
%          'corresponding' which correlates each trace of "seis1" with the 
%                corresponding trace of "seis2". Thus "seis1" and "seis2" must 
%                have the same number of traces.
%          Default: {'option','corresponding'}
% OUTPUT
% corr     seismic structure; correlation of the two input data sets
%          if keyword 'type' is 'all', then two header mnemonics (defaults: 'seis1' and 'seis2') 
%          are created which indicate, for each output trace, which trace from each input data set 
%          was used
%          If the second input data set consists of one trace only then the headers of the first
%          data set are copied to the output.
%          If the first input data set consists of one trace only and the second data set has more 
%          than one trace then the headers of the second data set are copied to the output. 
% aux      structure with field
%      'shifts'  vector of shifts to apply to the second input dataset to align
%          it with the first
% EXAMPLE
%          seismic=s_data;
%          corr=s_correlate(seismic,seismic,{'lags',-100,100},{'normalize','traces'});
%          s_wplot(corr)

global S4M

%     Set default values for input arguments
param.normalize='traces';
param.lags=[];
param.option='corresponding';
param.headers={'seis1','seis2'};

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);


%       Perform error checking
ds1=seis1.name;
ds2=seis2.name;

if ~istype(seis1,'seismic') || ~istype(seis2,'seismic')
   error(' The first two input arguments must be seismic structures.')
end
if seis1.step ~= seis2.step
   error(' The two input datasets must have the same sample interval.')
end

if isnull(seis1)
   seis1=s_rm_trace_nulls(seis1);
   disp(['Dataset "',ds1,'" has null values; they have been replaced by zeros.'])
end

if isnull(seis2')
   seis2=s_rm_trace_nulls(seis2);
   disp(['Dataset "',ds2,'" has null values; they have been replaced by zeros.'])
end
temp=(seis1.first-seis2.first)/seis1.step;
if abs(round(temp)-temp) > 1.0e6*eps
    error([' The start time difference of the two input datasets ' ...
           'must be an integer multiple of the sample interval.'])
end

if isnull(seis1) || isnull(seis2)
    error(' Null values are not allowed in seismic input data sets')
end

if isempty(param.lags)
   tt1=seis1.last-seis1.first;
   tt2=seis2.last-seis2.first;
   up=seis1.first-seis2.first;
   down=seis1.last-seis2.last;
   if tt1 < tt2 
      temp=up;
      up=down;
      down=temp;
   end

elseif length(param.lags) == 1
   down=abs(fix(param.lags/seis1.step)*seis1.step);
   up=-down;

elseif length(param.lags) == 2
    up=round(param.lags{1}/seis1.step)*seis1.step;
    down=round(param.lags{2}/seis1.step)*seis1.step;
    if up > down
       error([' The second parameter of keyword "times" (',num2str(down), ...
            ') must not be smaller than the first (',num2str(up),').'])
    end 

else
    error(' Keyword "lag" must have one or two parameters')
end


%       Initialize output dataset
corr.type='seismic';
corr.tag='unspecified';
corr.name='Cross correlation';
corr.first=up;
corr.last=down;
corr.step=seis1.step;
corr.units=seis1.units;
         
ntr1=size(seis1.traces,2);
ntr2=size(seis2.traces,2);

first1=max([seis1.first,seis2.first+up]);
last1=min([seis1.last,seis2.last+down]);
ia=(first1-seis1.first)/seis1.step+1;
ie=(last1-seis1.last)/seis1.step;

nup=round((up-first1+seis2.first)/seis1.step);
ndown=round((down-first1+seis2.first)/seis1.step);

traces1=seis1.traces(ia:end+ie,:);

if isempty(traces1)
   disp([' Time ranges for which "',ds1,'" and "',ds2,'" are defined do not overlap for'])
   disp([' the requested shifts (',num2str(up),' to ',num2str(down),' ',seis1.units,').'])
   t1 = seis1.first - seis2.first;
   t2 = t1+abs(seis1.last - seis2.last - t1);
   up=min([t1,t2]);
   down=max([t1,t2]);
   disp([' Suitable shifts would be ',num2str(up),' to ',num2str(down),' ',seis1.units])
   pause(0)
   error(' Abnormal termination')
end

if ntr2 == 1         % Copy rest of fields from first dataset
   corr=copy_fields(seis1,corr);
elseif ntr1 == 1     % Copy rest of fields from second dataset
   corr=copy_fields(seis2,corr);
end

if strcmpi(param.option,'all')    % Put trace numbers into headers of output dataset
   corr.traces=xcorr_all(traces1,seis2.traces,nup,ndown,param.normalize);
   headerv1=0:ntr1*ntr2-1;
   headerv2=fix(headerv1/ntr1)+1;
   headerv1=mod(headerv1,ntr1)+1;
   corr=ds_header(corr,'add',param.headers{1},headerv1,'na',['Trace number of "',ds1,'"']);
   corr=ds_header(corr,'add',param.headers{2},headerv2,'na',['Trace number of "',ds2,'"']);
   htext=['Correlation of all traces of "',ds1,'" and "',ds2,'"'];

else
   if ntr1 ~= ntr2
      error([' For this option ',ds1,' and ',ds2,' must have the same number of traces'])
   end
   corr.traces=xcorr_corresp(traces1,seis2.traces,nup,ndown,param.normalize);
   htext=['Correlation of corresponding traces of "',ds1,'" and "',ds2,'"'];
end

%       Copy rest of fields
corr=copy_fields(seis1,corr);

%       Shifts required for alignment
if nargout > 1
   [dummy,index]=max(corr.traces);   %#ok First output argument is not required
   aux.shifts=up+(index-1)*corr.step;
end

%       Append history field
if S4M.history && isfield(seis1,'history')
   corr=s_history(corr,'append',htext);
end
