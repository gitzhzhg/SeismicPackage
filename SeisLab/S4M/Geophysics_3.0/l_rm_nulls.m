function wlog=l_rm_nulls(wlog,action,mnemonics)
% Function removes leading and trailing rows from log curves if they contain 
% null values; with option "anywhere", every row with a NaN in one of the 
% listed curves is removed.
% Null values bracketed by non-null values may be retained. The function assumes
% that null values are represented by NaNs. If this is not the case they are 
% replaced by NaNs in the output structure.
%
% Written by:  E. Rietsch: March 6, 2000
% Last updated: October 2, 2007:  Generalize input to handle dataset vectors; bug fixes
%
%        wlog=l_rm_nulls(wlog,action,mnemonics)
% INPUT
% wlog   log structure or structure vector
% action  optional parameter which controls action to be performed; possible values are:
%        'all'  leading and trailing rows are removed if the non-depth 
%               curves listed in "mnemonics" are all NaN  (DEFAULT).
%        'any'  leading and trailing rows are removed if any of the non-depth 
%               curves listed in "mnemonics" has a NAN value
%        'anywhere' every row with a NaN in one of the curves listed 
%               in "mnemonics" is removed (not just the leading/trailing rows) 
%        Default: action='all'
% mnemonics  optional cell array of mnemonics to be considered 
%        if not given or if empty then all curves are used
%        Default: mnemonics=[];
% OUTPUT
% wlog   output log structure
%
% EXAMPLE
%        wlog=l_data;   % Create log structure with null values
%        wlog.curves([1:4,10:20],2)=NaN;
%        wlog.null=NaN;
%
%        wlog1=l_rm_nulls(wlog,'anywhere',{'DTp','RHO'});
%        [isnull(wlog),isnull(wlog1)]     % Must be [true,false]
%
%        wlog2=l_rm_nulls(wlog,'any',{'DTp','RHO'});
%        [isnull(wlog),isnull(wlog2)]     % Must be [true,true]


if ~istype(wlog,'well_log')
   error('First input argument must be a well log.')
end

if nargin == 1
   mnemonics=[];
   action='all';

elseif nargin == 2
   mnemonics=[];
end

for ii=length(wlog):-1:1
   wlog(ii)=rm_nulls_no1(wlog(ii),action,mnemonics);   %#ok Loop starts with largest element
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=rm_nulls_no1(wlog,action,mnemonics)

global S4M

if ~isnull(wlog)   % If no null values are present in log curves ...   
   return
end

m=size(wlog.curves,2);
if nargin < 2 || isempty(action)
   action='all';
end

if nargin < 3 || isempty(mnemonics)
   idx=2:m;
else
   if ischar(mnemonics)
      mnemonics={mnemonics};
   elseif ~iscell(mnemonics)
      error(' Input parameter mnemonics must be a string or a cell array of strings')
   end
   nm=length(mnemonics);
   if S4M.case_sensitive
      idx=find(ismember(wlog.curve_info(:,1),mnemonics));
   else
      idx=find(ismember(lower(wlog.curve_info(:,1)),lower(mnemonics)));
   end
  
   if length(idx) ~= nm
      disp([char(13),' Curves requested:'])
      disp(mnemonics)
      disp(' Curves in log structure:')
      disp(wlog.curve_info(:,1)')
      error(' Not all requested curves available in log structure')
   end
end

if ~isempty(wlog.null) && ~isnan(wlog.null)  % Replace numeric null value (e.g. -999.25) by NaN 
   wlog.curves(wlog.curves == wlog.null)=NaN;
   wlog.null=NaN;   
end

switch action
case {'any','anywhere'}
   test=sum(wlog.curves(:,idx),2);

case 'all'
   test=min(wlog.curves(:,idx),[],2);

otherwise
   error(['Action "',action,'" is not defined'])

end

switch action
case {'anywhere'}
   wlog.curves=wlog.curves(~isnan(test),:);

otherwise
   ia=find(~isnan(test),1);
   ie=find(~isnan(test),1,'last');
   wlog.curves=wlog.curves(ia:ie,:);
end

wlog.first=wlog.curves(1,1);
wlog.last=wlog.curves(end,1);
wlog.step=depths2step_with_checking(wlog.curves(:,1));


%	Check if "wlog.null" needs to be changed
if all(all(~isnan(wlog.curves(:,2:end))))
   wlog.null=[];
end
