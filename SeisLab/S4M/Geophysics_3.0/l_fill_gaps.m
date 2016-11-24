function wlog=l_fill_gaps(wlog,mnemonics)
% Function interpolates across gaps in the log curves (null values) of all 
% curves specified by an optional list of mnemonics. The function assumes 
% that null values are represented by NaNs.
%
% Date April 29, 2000;  written by E. Rietsch
% Last update: June 8, 2004: use "wlog" instead of "log" and "log_out"
%
%             wlog=l_fill_gaps(wlog,mnemonics)
% INPUT
% wlog        log structure
% mnemonics   mnemonic or cell array of mnemonics to interpolate
%             if "mnemonics" is not given or empty all curves are
%             interpolated
% OUTPUT
% wlog       log with interpolated curves; curves not in the list of mnemonics are
%            copied unchanged

if ~istype(wlog,'well_log')
   error('First input argument must be a well log')
end

if ~isfield(wlog,'null')       % If there are no null values in the log curves ...
   return
end

[n,m]=size(wlog.curves);

if nargin < 2 || isempty(mnemonics)
   idx=2:m;
else
   if ischar(mnemonics)
      mnemonics={mnemonics};
   elseif ~iscell(mnemonics)
      error(' Input parameter mnemonics must be a string or a cell array of strings')
   end
   nm=length(mnemonics);
   idx=find(ismember(lower(wlog.curve_info(:,1)),lower(mnemonics)));
   if length(idx) ~= nm
      disp(' Curves_requested:')
      disp(mnemonics)
      disp(' Curves_found:')
      disp(cell2str(wlog.curve_info(:,1),', '))
      error('Number of curves found differs from number requested.')
   end
end

if ~isnan(wlog.null)
   idx=find(wlog.curves == wlog.null);
   wlog.curves(idx)=NaN;
   wlog.null=NaN;   
end

% wlog.curves=wlog.curves;

for ii=idx(:)'
  index=find(~isnan(wlog.curves(:,ii)));
  if ~isempty(index) && length(index) ~= n 
     wlog.curves(index(1):index(end),ii)=fill_gaps(wlog.curves(index(1):index(end),1), ...
                  wlog.curves(index(1):index(end),ii),index-index(1)+1);
  end
end
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function curve=fill_gaps(depth,curve,index)
% Function fills gaps in log curve.
% INPUT
% depth     uniform depth values
% curve     corresponding values of the curve
% index     index vector of non-NaN values (i.e. curve(index) is not NaN)
% OUTPUT
% curve     input curve with the NaNs replaced by interpolated values

idx=find(diff(index) > 1);
if isempty(idx)
   return
end
nidx=length(idx);
for ii=1:nidx
   ia=index(idx(ii));
   ie=index(idx(ii)+1);
   curve(ia+1:ie-1)=interp1q(depth([ia,ie]),curve([ia,ie]),depth(ia+1:ie-1));
end
