function wfilter=s_wiener_filter(from,to,varargin)
% Function computes the filter that matches (filters that match) the first
% input data set to the second input data set. There are several options
% which are described below under keyword "options".
%
% Written by: E. Rietsch: October 2001
% Last updated: April 25, 2006: Fixed bug in "mfilter_d2d" 
% 
%             wfilter=s_wiener_filter(from,to,varargin)
% INPUT
% from, to    seismic data sets to be matched. 
%             Depending on the options used, the two data sets may need to
%             have the same number of traces.
%             If the length of "to" + the filter length is less than the
%             length of "from" a filter is computed for a number of shifts 
%             equal to "length ("to") - length ("from") - length("wfilter");
%             the best-matching filter is chosen as output.
% varargin    one or more cell arrays; the first element of each cell
%             array is a keyword,the other elements are parameters. 
%             Presently, keywords are:
%       'method'    defines the method/algorithm to be used to compute
%              the filter. Presently,the following algorithms are available:
%             'sridge'   simple ridge regression            
%       'option'    specifies which type of filter to compute. Presently,
%             the following options are available:
%             'tracewise'  compute filters (one filter for each input/output trace pair)
%                   which convert traces in the first input dataset into the
%                   corresponding traces of the second seismic structure input. This
%                   requires that the the two seismic structures have the same number of 
%                   traces (ntr1=ntr2). The number of filters output is equal to ntr1, the 
%                   number of traces in the input data sets.
%             'dataset'  Compute a single filter that converts every trace 
%                   in the first input dataset into the corresponding trace
%                   of the second seismic input dataset. This requires 
%                   that the the two seismic structures have the same number 
%                   of traces (ntr1=ntr2).
%             'all'    Compute ntr1*ntr2 individual filters which match each
%                   trace of the first input dataset to each trace 
%                   of the second input data set.
%              No default unless "from" and "to" are one-trace datasets;
%              in this case {'option','tracewise'}      
%       'flength'   filter length in ms; no default.
%       'window1'   window on the first input data set which should be used
%             Default: {'window1',from.first,from.last}
%       'window2'   window on the second input data set that should be matched             
%             Default: {'window1',to.first,to.last}
%       'null'      Null value to use for filter coefficients that do not exist
%             Only important if there is more than one filter and if 
%             filters are shifted relative to one another. 
%             Default: {'null',0}
%       'header'    As part of the filter calculation the cross-correlation
%             of the filtered first input data set, i.e. "filter*from", 
%             and second data set, "to", is computed. 
%             'header' defines the header name used to store 
%             these cross-correlations for option "tracewise". To suppress 
%             creating this header set {'header',[]}
%             Default: {'header','param_cc'}
%       'wnoise'    fraction of "white noise" to add
%             Default: {'wnoise',0.01}
% OUTPUT
% wfilter      filter in form of a seismic structure. Filter has either one trace (a single 
%             filter matching corresponding traces), or as many traces as there are traces
%             in from or to. In this case there is one filter for each pair of
%             corresponding input traces. Finally there can be ntr1*ntr2 filters matching
%             trace of the first input data set to each trace of the second input data set.
%             In this case the first ntr2 filters match the first trace of the first input 
%             data set to the ntr2 traces of the second data set, the second ntr2 filters 
%             match the second trace of the first input data set to the second data set, etc.


%      Check input data
if ~istype(from,'seismic')  ||  ~istype(to,'seismic')
   error(' The first two input data sets must be seismic structures.')
end

ntr1=size(from.traces,2);
ntr2=size(to.traces,2);

if from.step ~= to.step
   error([' The two input data sets must have the same sample interval (', ...
          num2str(from.step),' differs from ',num2str(to.step),')'])
end

%    Set default values for input parameters
param.increment=from.step;
param.flength=[];
%param.method='sridge';
param.null=0;
param.option=[];
param.stepout=[];
param.scale='yes';
% param.type=1;
param.window1={from.first,from.last};
param.window2={to.first,to.last};
param.header='param_cc';
param.wnoise=0.01;

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

if isempty(param.option) 
   if size(from.traces,2) == 1  &&  size(to.traces,2) == 1
      param.option='tracewise';
   else
      error('Parameter "option" must be specified.')
   end
end

if isempty(param.option)
   error('Parameter "option" must be specified.')
end

if isempty(param.flength)
   error(' Parameter "flength" has no default; must be specified')
else
   nf=round(param.flength/from.step)+1;
%   nf2=round((nf+1)/2);
end


if strcmp(param.scale,'yes')
   scaling=true;
else
   scaling=false;
end


ta1=max([param.window1{1},from.first]);
te1=min([param.window1{2},from.last]);
ta2=max([param.window2{1},to.first]);
te2=min([param.window2{2},to.last]);
ia1=round((ta1-from.first)/from.step)+1;
ie1=round((te1-from.first)/from.step)+1;
ia2=round((ta2-to.first)/to.step)+1;
ie2=round((te2-to.first)/to.step)+1;
n1=ie1-ia1+1;
n2=ie2-ia2+1;

if isempty(param.stepout)
   param.stepout=param.flength/10;
end 
incr=max(1,round(param.stepout/from.step));

wfilter.type='seismic';
wfilter.tag='wavelet';
wfilter.name='Filter';


% 	Compute Wiener filter.

switch param.option

%------------------------------------------------

               case 'tracewise'

if ntr1 ~= ntr2
   error([' The two input data sets must have the same number of traces (', ...
          num2str(ntr1),' differs from ',num2str(ntr2),')'])
end


if n2 > n1-nf  % Number of samples of target > number of samples of original
                % data set - filter coefficients 

   [filt,cc_max,shifts]=mfilter_t2t(from.traces(ia1:ie1,:), ...
                        to.traces(ia2:ie2,:),nf,param.wnoise,incr);
   min_shift=min(shifts);
   dshift=(shifts-min_shift);
   max_shift=max(dshift);
   if max_shift > 0  &&  isnan(param.null)
      wfilter.null=NaN; 
      wfilter.traces=NaN*ones(nf+max_shift,ntr1);
   else
      wfilter.traces=param.null*ones(nf+max_shift,ntr1);
   end

   for ii=1:ntr1
      wfilter.traces(1+dshift(ii):dshift(ii)+nf,ii)=filt(:,ii);
   end
   
else 
   disp(['No of samples of target (',num2str(n2),') < '])
   disp(['No of samples of original (',num2str(n1),') - No of filter coefficients (',num2str(nf),')'])
   alert(' ... not yet sufficiently tested for multi-trace filters (requires shifting of original)')

   nshifts=n1-nf-n2;
   ia10=ia1-1;
   ie10=ia10+n2+nf-2;
   cc=zeros(nshifts,ntr1);
   filters=zeros(nshifts,ntr1,nf);
   for ii=1:nshifts
      [filt,cc_max,shifts]=mfilter_t2t(from.traces((ia10:ie10)+ii,:), ...
                        to.traces(ia2:ie2,:),nf,param.wnoise,incr); %#ok This is an untested option
      filters(ii,:,:)=filt';
      cc(ii,:)=cc_max;
   end
   [cc_max,idx]=max(cc);
   idx1=idx+(0:nshifts:(ntr1-1)*nshifts);
   shifts=2-idx;
   filters=reshape(filters,[],nf);
   filters=filters(idx1,:)';

   min_shift=min(shifts);
   dshift=(shifts-min_shift);
   max_shift=max(dshift);
   if max_shift > 0  &&  isnan(param.null)
      wfilter.null=NaN; 
      wfilter.traces=NaN*ones(nf+max_shift,ntr1);
   else
      wfilter.traces=param.null*ones(nf+max_shift,ntr1);
   end

   for ii=1:ntr1
      wfilter.traces(1+dshift(ii):dshift(ii)+nf,ii)=filters(:,ii);
   end
 
end

wfilter.step=from.step;
%filter.first=to.first-from.first-(nf-nf2-min_shift)*filter.step;
%filter.first=to.first-from.first-(nf-min_shift)*filter.step;
wfilter.first=ta2-ta1-(nf-min_shift)*wfilter.step;
wfilter.last=wfilter.first+(nf+max_shift-1)*wfilter.step;
if isfield(from,'headers')
   wfilter.headers=from.headers;
   wfilter.header_info=from.header_info;
end

% -----------------------------------------------

               case 'dataset'

if ntr1 ~= ntr2
  error([' The two input data sets must have the same number of traces (', ...
          num2str(ntr1),' differs from ',num2str(ntr2),')'])
end
if n2 >= n1-nf
   [filt,cc_max,shifts]=mfilter_d2d(from.traces(ia1:ie1,:), ...
                        to.traces(ia2:ie2,:),nf,param.wnoise,incr);
   wfilter.traces=filt;
else
   error(' Second data set too short (< length of "from" + filter length); not yet implemented')
end

wfilter.step=from.step;
wfilter.first=ta2-ta1-(nf-shifts)*wfilter.step;
wfilter.last=wfilter.first+(nf-1)*wfilter.step;

% -----------------------------------------------

               case 'all'

ntr12=ntr1*ntr2;
filters=zeros(nf,ntr12);
cc_max=zeros(ntr12,1);
shifts=zeros(ntr12,1);

if n2 >= n1-nf
   for ii=1:ntr1
      [filt,cc_m,shift,scale]=mfilter_t2d(from.traces(ia1:ie1,ii), ...
                        to.traces(ia2:ie2,:),nf,param.wnoise,incr,0);
      if scaling
%        filters(:,(ii-1)*ntr2+1:ii*ntr2)=filt*spdiags(scale,0, ...
%            length(scale),length(scale));
         filters(:,(ii-1)*ntr2+1:ii*ntr2)=mvt(filt,scale);
      else
         filters(:,(ii-1)*ntr2+1:ii*ntr2)=filt;
      end
      cc_max((ii-1)*ntr2+1:ii*ntr2)=cc_m;
      shifts((ii-1)*ntr2+1:ii*ntr2)=shift;
   end
   min_shift=min(shifts);
   dshift=(shifts-min_shift);
   max_shift=max(dshift);
   if max_shift > 0 && isnan(param.null)
      wfilter.null=NaN; 
      wfilter.traces=NaN*ones(nf+max_shift,ntr1*ntr2);
   else
      wfilter.traces=param.null*ones(nf+max_shift,ntr1*ntr2);
   end
   for ii=1:ntr12
      wfilter.traces(1+dshift(ii):dshift(ii)+nf,ii)=filters(:,ii);
   end
else
   error(' Not yet implemented')
end

wfilter.step=from.step;
% filter.first=to.first-from.first-(nf-nf2-min_shift)*filter.step;
wfilter.first=to.first-from.first-(nf-min_shift)*wfilter.step;
wfilter.last=wfilter.first+(nf+max_shift-1)*wfilter.step;
cc_max=reshape(cc_max,ntr2,ntr1);

% -----------------------------------------------

               otherwise

error([' Unknown option "',param.option,'"'])

end		% End of switch block


wfilter.units=from.units;

if ~isempty(param.header)  &&  ~strcmpi(param.option,'dataset')
   wfilter=ds_header(wfilter,'add',param.header,cc_max(:)','n/a', ...
              'Coefficient of correlation');
end

%    Append history field
if isfield(from,'history')  &&  isfield(to,'history')
   wfilter.history=from.history;
   htext=param.option;
   wfilter=s_history(wfilter,'append',htext);
   wfilter=s_history(wfilter,'merge',to.history);
end 
 
