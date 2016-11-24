function wlog=l_sort(wlog,mnem,varargin)
% Sort curves of a log so that a specified curve increases monotonically;
% since the "depth" column of a log needs to increase strictly monotonically 
% a new curve "depth curve", called "index", is created (unless it already exists)
% So, generally, the output log has one more curve than the input log.
%
% Written by: E. Rietsch: August 3, 2003
% Last updated:
%
%        wlog=l_sort(wlog,mnem,varargin)
% INPUT
% wlog   well log
% mnem   mnemonic of the curve to be sorted
% varargin one or more cell arrays; the first element of each cell array is a 
%        keyword string, the following arguments contains a parameter(s).
%        Accepted keywords are:
%    'sortdir'   Sort direction. Possible values are 'increasing' and 'decreasing'
%                 Default: {'sortdir','increasing'}
% OUTPUT
% wlog  log with sorted columns
%
% EXAMPLE
%    wlog=l_data;
%    wlog1=l_sort(wlog,'dtp',{'sortdir','decreasing'});
%    l_plot(wlog,{'curves',[]})

%       Defaults of input arguments
param.sortdir='increasing';

%       Use input parameters to change defaults
param=assign_input(param,varargin);

if nargin == 1
   error(' One curve must be specified')
end

if any(ismember(wlog.curve_info,'index'))
   ButtonName=questdlg('The mnemonic "index" alredy exists. Do you want to replace it?', ...
                       'SeisLab: l_sort', ...
                       'yes','no','no');
   if strcmp(ButtonName,'no')
      disp(' The mnemonic "index" alredy exists.')
      error('Abnormal termination')
   end
   wlog=l_curve(wlog,'delete','index');
end
[dummy,idx]=sort(l_gc(wlog,mnem));   %#ok First output argument is not required

if strcmpi(param.sortdir,'decreasing')
   idx=idx(end:-1:1);
elseif strcmpi(param.sortdir,'increasing')
   % do nothing
else
   error(' Unknown sort direction');
end

nsamp=size(wlog.curves,1);
wlog.curves=[(1:nsamp)',wlog.curves(idx,:)];
wlog.curve_info=[{'index','n/a','Depth index'};wlog.curve_info];
wlog.first=1;
wlog.last=nsamp;
wlog.step=1;
