function seisout=s_rm_traces(seismic,option)
% Function removes traces 
%       which consist exclusively of null values or 
%       which have at least one null value
% An alert is issued if no trace is output
% See also: "s_rm_trace_nulls"
%
% Written by: E. Rietsch: January 6, 2001
% Last updated: February 2, 2001: remove common leading and trailing nulls prior to applying selection criterion
%
%           seisout=s_rm_traces(seismic,option)
% INPUT
% seismic   seismic data set
% option    option requested. Possible values are:
%           'any'  a trace is discarded if it contains any null values
%           'all'  a trace is discarded if all values are null values
%           Default: 'all'
%

global S4M

seisout=seismic;
if ~isnull(seismic)
   return
end

if nargin == 1
   option='all';
end

[nsamp,ntr]=size(seismic.traces);

%       First remove any leading or trailing NaNs common to all traces
test=max(seismic.traces,[],2);
index=find(~isnan(test));
if isempty(index)
   seisout=rmfield(seisout,{'traces','headers','header_info','null'});
   disp(' Alert from "s_rm_traces": Seismic traces have only null values')
   return
end
seisout.traces=seismic.traces(index(1):index(end),:);
seisout.first=seismic.first+(index(1)-1)*seismic.step;
seisout.last=seismic.last-(nsamp-index(end))*seismic.step;

index=sum(isnan(seisout.traces));

switch option

case 'all'

idx=find(index ~= nsamp);
if isempty(idx)
   seisout=rmfield(seisout,{'traces','headers','header_info','null'});
   disp(' Alert from "s_rm_traces": Seismic traces have only null values')
   return
end
seisout.traces=seisout.traces(:,idx);
seisout.headers=seisout.headers(:,idx);
if sum(index(idx) == 0)
  seisout=rmfield(seisout,'null');
end
htext=[num2str(ntr-length(idx)),' traces (with all null values) removed'];


case 'any'

idx=find(index == 0);
if isempty(idx)
   seisout=rmfield(seisout,{'traces','headers','header_info','null'});
   disp(' Alert from "s_rm_traces": Seismic traces have only null values')
   return
end
seisout.traces=seisout.traces(:,idx);
seisout.headers=seisout.headers(:,idx); 
seisout=rmfield(seisout,'null');
htext=[num2str(ntr-length(idx)),' traces (with at least one null value) removed'];


otherwise

error([' Unknown option: ',option])

end		% End of switch block


if  S4M.history && isfield(seismic,'history')
   seisout=s_history(seisout,'append',htext);
end
