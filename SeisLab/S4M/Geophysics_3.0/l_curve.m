function wlog=l_curve(wlog,action,mnem,values,units,description)
% Function manipulates curve(s) of a log structure
%
% Written by: E. Rietsch: May 12, 2000
% Last updated: November 8, 2004: update also field "curve_types (if it exists).                
%
%          wlog=l_curve(wlog,action,mnem,values,units,description)
%          wlog=l_curve(wlog,action,mnem)    for action='delete','delete_ne',
%                                                       'keep','keep_ne','list'
%          wlog=l_curve(wlog)                assumes action 'list'
% INPUT
% wlog      log structure;
%           If this is the only input argument, "action" is set to 'list' and
%           "mnem" is set to '*'. The function prints a list of all curve 
%           values as described below. 
% action    Defines action to take. Possible values are:
%           'add'      Add curve with mnemonic mnem. Gives error message if 
%                      curve already exists
%           'add_ne'   Add curve with mnemonic mnem. Replaces it if it 
%                      already exists.
%           'replace'  Replaces curve with mnemonic mnem; error if curve
%                      does not exist; original units and description are
%                      retained if not given
%           'delete'   Delete curve(s) with mnemonic(s) mnem; 
%                      error if any of the curves do not exist 
%           'delete_ne' Delete curve(s) with mnemonic(s) mnem if the curve is 
%                      (curves are) present; no error if one or more of the 
%                      curve(s) specified does not exist 
%           'keep'     Keep curve(s) with mnemonic(s) mnem and deletes all
%                      others; error if one or more of the curve(s) do not
%                      exist 
%           'keep_ne'  Keep curve(s) with mnemonic(s) mnem if it is (they are)
%                      present and deletes all others; no error if any or all
%                      curves specified are not present
%           'rename'   Rename curve mnemonic, keep everything else the same
%           'list'     Print short list: for specified curve mnemonic(s):
%                      it lists minimum and maximum value, number of gaps
%                      (null values) between first and last non-null value,
%                      total number of missing samples (null values), units 
%                      of measurement, and curve description
%           
%          The other input parameters depend on the parameter "action"
%                   CASE action = 'add', 'add_ne', or 'replace'
% mnem      curve mnemonic
% values    curve values; if only one value is given the curve is
%           assumed to be constant
% units     Units of measurement for curve values (optional if 'action' 
%           is 'replace')
% description   Description of curve mnemonic (optional if 'action' is 'replace')
%           If "action" is 'replace' "units" and "description" need not 
%           be given; in this case the original "units" and "description"
%           are retained.
%
%                CASE action = 'delete', delete_ne', 'keep', 'keep_ne', or 'list'
% mnem      curve mnemonic or cells array with curve mnemonics
%           '*' means all curves
%
%                CASE action = 'rename'
% mnem      Cell array consisting of two strings: the old and 
%           the new name of the curve
%
% OUTPUT
% wlog      "Updated" log structure (log structure is unchanged if action == 'list')
%
%
% EXAMPLE
%           wlog=l_data;
%           l_curve(wlog)
              
if nargin < 1
  error('At least one input argument (log structure) required')
end

if ~isstruct(wlog)
  error('The first input argument must be a log structure')
end

if nargin == 1
  action='list';
  mnem={'*'};
else
  action=lower(action);
  if ~iscell(mnem)
    mnem={mnem};
  end
end
% mnem=lower(mnem);

[mh,nh]=size(wlog.curves);

lmnem=length(mnem);
if lmnem == 1 && strcmp(char(mnem),'*') && ...
   ~strcmp(action,'add') && ~strcmp(action,'add_ne') && ~strcmp(action,'replace')
  mnem=wlog.curve_info(:,1);
  lmnem=length(mnem);
end

switch action
               case {'add','add_ne','replace'}

lv=length(values);
if lv == 1
   val=values ; 
elseif lv == mh
   val=reshape(values,mh,1);
else
   error(['Input argument "values" must be a constant or a vector ', ...
         'whose length is equal to the number of samples in the log structure'])
end

if ~isfield(wlog,'curve_info')
   error([' Field ''curve_info'' is missing from log structure "',inputname(1),'"'])
else
   [idx,dummy]=curve_index1(wlog,char(mnem)); %#ok Second argument used to 
                                              % indicate error handling outside
end
if ~isempty(idx)
   if strcmpi(action,'add')
      error(['The curve mnemonic ',char(mnem),' already exists in the log.'])
   else
      wlog.curves(:,idx)=val;
      if strcmpi(action,'replace') && nargin <= 4
         disp([' Original units of measurement and description of "',char(mnem),'" retained'])
      else
         wlog.curve_info(idx,2)={units};
         wlog.curve_info(idx,3)={description};
      end
   end
   if idx == 1   % Depth column replaced; create new fields "first", "last", "step"
      wlog.first=wlog.curves(1,1);
      wlog.last=wlog.curves(end,1);
      wlog.step=depths2step(wlog.curves(:,1));
   end 
      
else
   if strcmpi(action,'replace')
      error(['The curve mnemonic ',char(mnem),' does not exist in the log'])
   else
      idx=nh+1;
      wlog.curve_info(idx,1)=mnem;
      wlog.curves(:,idx)=val;
      wlog.curve_info(idx,2)={units};
      wlog.curve_info(idx,3)={description};
   end      
end

              case {'delete','delete_ne'}
lmnem=length(mnem);
for ii=1:lmnem
  if strcmpi(action,'delete');
    idx=curve_index1(wlog,mnem{ii});
  else
    [idx,dummy]=curve_index1(wlog,mnem{ii});  %#ok Second argument used to 
                                              % indicate error handling outside
  end
  if ~isempty(idx)                        
    if length(idx) == length(wlog.curve_info(:,1))
      wlog=rmfield(wlog,{'curves','curve_mnem','curve_units','curve_descriptions'});
    else
      wlog.curves(:,idx)=[];
      wlog.curve_info(idx,:)=[];
    end
  end
end 
if isfield(wlog,'curve_types')
   bool=ismember(lower(wlog.curve_types(:,1)),lower(mnem));
   wlog.curve_types(bool,:)=[];
end

              case {'keep','keep_ne'}
idx=find(~ismember(lower(wlog.curve_info(2:end,1)),lower(mnem)))+1;
if lmnem ~= nh-length(idx)-1 && strcmp(action,'keep')
   for ii=1:lmnem
      if ~ismember(lower(wlog.curve_info(:,1)),lower(mnem(ii)))
         disp([' Curve "',mnem{ii},'" does not exist in the log'])
      end
   end
   error(' Abnormal termination')
end
if length(idx) == nh
   wlog=rmfield(wlog,{'curves','curve_info'});
else
   wlog.curves(:,idx)=[];
   wlog.curve_info(idx,:)=[];
end 
if isfield(wlog,'curve_types')
   bool=ismember(lower(wlog.curve_types(:,1)),lower(mnem));
   temp=wlog.curve_types(bool,:);
   if ~isempty(temp)
      wlog.curve_types=temp;
   end
end

              case 'rename'
if lmnem ~= 2
   error(['Parameter "mnem" must be a cell array with two strings' , ... 
        '(old curve mnemonic and new curve mnemonic)'])
elseif ismember(lower(wlog.curve_info(:,1)),lower(mnem(2)))
   error(['New curve mnemonic ',char(mnem(2)),' already exists in the log'])
else
   idx=find(ismember(lower(wlog.curve_info(:,1)),lower(mnem(1))));
   if isempty(idx)
      error([' Log curve ',char(mnem(1)),' does not exist'])
   else
      wlog.curve_info(idx,1)=mnem(2);
   end
end
if isfield(wlog,'curve_types')
   idx=find(ismember(lower(wlog.curve_types(:,1)),lower(mnem{1})));
   if ~isempty(idx)
      wlog.curve_types{idx,1}=mnem{2};
   end
end


              case 'list'
idx=find(ismember(lower(wlog.curve_info(:,1)),lower(mnem)));
if isempty(idx)
  error([' Log curve(s) "',cell2str(mnem,'", "'),'" do(es) not exist'])
end
mnems=char('MNEMONIC',wlog.curve_info{idx,1});
descr=char('DESCRIPTION',wlog.curve_info{idx,3});
units=char('UNITS',wlog.curve_info{idx,2});
hmin=min(wlog.curves(:,idx))';
hmax=max(wlog.curves(:,idx))';

%    Compute number of gaps and total number of NaNs in each curve
lidx=length(idx);
gaps=zeros(lidx,1);
nnans=zeros(lidx,1);

for ii=1:lidx
  idxnan=find(~isnan(wlog.curves(:,idx(ii))));
  if length(idxnan) > 1
    didx=diff(idxnan);
    gaps(ii)=sum(didx > 1);
    nnans(ii)=mh-length(idxnan);
  end
end
  
spaces(1:length(hmax)+1)=' ';

shmin=char('MIN_VAL',num2str(hmin));
shmax=char('MAX_VAL',num2str(hmax));
sgaps=char('GAPS',num2str(gaps));
snans=char('NaNs',num2str(nnans));
if ~isempty(inputname(1))
   disp(['Curves of log structure "',inputname(1),'"'])
end

disp([mnems,spaces',shmin,spaces',spaces',shmax,spaces', ...
      spaces',sgaps,spaces',snans,spaces',units,spaces',descr]);

%   Write message if one or more of the curves specified were not found
if lmnem ~= length(idx)    
   for ii=1:lmnem
      if ~ismember(lower(wlog.curve_info(:,1)),lower(mnem(ii)))
         disp([char(mnem(ii)),' is not present'])
      end
   end
end

ier=l_check(wlog);
if ier
   error('Abnormal termination')
end

if nargout == 0
   clear wlog
end

              otherwise
error(['Action "',action,'" is not defined'])


end
