function seismic=s_rm_trace_nulls(seismic,option)
% Remove NaN's at the beginning and end that are common to all traces;
% replace any other NaN's with zeros or leave them unchanged (see 
% argument "option").
% No action is taken if the field 'null' is [];
%
% Written by: E. Rietsch: July 12, 2000
% Updated: April 2, 2007: Generalize to vectors of seismic datasets
%
%            seismic=s_rm_trace_nulls(seismic,option)
% INPUT
% seismic    seismic data set
% option     logical variable; If true all NaN's left after those at the 
%            beginning and end of the traces have been removed are replaced 
%            by zeros;  if false those NaN's are left in the data
%            Default: option=true
% OUTPUT
% seismic    seismic after NaN removal
%

% UPDATE HISTORY
%            November 22, 2006: Set "null" field to empty matrix; streamline
%            code


global ABORTED S4M  

if nargin == 1
   option=true;
end

%     Handle vectors of seismic datasets
nds=length(seismic);
if nds > 1
   for ii=1:nds
      seismic(ii)=s_rm_trace_nulls(seismic(ii),option);
   end
   return
end


if ~isnull(seismic)
   return
end

%	Remove NaN's
nsamp=size(seismic.traces,1);
bool=isnan(seismic.traces);
index=find(any(~bool,2));
if isempty(index) 
   if S4M.deployed
      disp(' Seismic traces have only null values.')
      msgdlg('Seismic traces have only null values.')
      ABORTED=true;
      return
   else
      error(' Seismic traces have only null values')
   end
end

%	Change start time and end time if necessary
if index(1) > 1  ||  index(end) < nsamp
   seismic.traces=seismic.traces(index(1):index(end),:);
   seismic.first=seismic.first+(index(1)-1)*seismic.step;
   seismic.last=seismic.last-(nsamp-index(end))*seismic.step;
end

%	Deal with "interior" NaN's (if there are any)
bool=bool(index(1):index(end),:);
if any(bool(:))
   if option
      seismic.traces(bool)=0;
      seismic.null=0;
   else
      seismic.null=NaN;
   end
else
   seismic.null=[];
end

%	Update processing history
seismic=s_history(seismic,'append', ...
        [num2str(nsamp-size(seismic.traces,1)),' samples removed']);

ABORTED=false;
