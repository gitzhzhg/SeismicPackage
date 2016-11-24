function wlog=l_select(wlog,varargin)
% Function retrieves subset of well log from a log structure. 
% If wlog is a structure vector the same subset is selected from each well log.
% An error message is printed if a column mnemonic requested is not found. 
% Column mnemonics are not case sensitive.
%
% Written by: E. Rietsch: October 7, 2000;
% Last updated: September 19, 2007: allow "wlog" to be a structure vector
%  
%           wlog=l_select(wlog,varargin)
% INPUT
% wlog	   well-log structure
% varargin  one or more cell arrays; the first element of each cell array is a 
%           keyword, the other elements are parameters. Presently, keywords are:
%       'curves'  strings with curve mnemonics. 
%           Default: {'curves','*'} (implies all curves)
%           The depth (first column of "wlog.curves") is always included  
%       'depths'  start and end depth of log segment (two comma-separated numbers
%           or a two-element vector)
%           Default: {'depths',wlog.first,wlog.last}
%       'rows'  string with logical expression involving one more of the 
%           curve mnemonics
%           Default: {'rows',''} (implies all rows)
%           Keywords 'curves', 'depths', and 'rows' may be given at the same time               
%
% OUTPUT
% wlog    output log with curves defined in "curves"
%
% EXAMPLES
%         wlog=l_data;
%         wlog1=l_select(wlog,{'curves','Vp'},{'rows','depth >= 9000'})
%         wlog2=l_select(wlog,{'curves','depth','Vp','Vs'},{'depths',10000,12000})
%         wlog3=l_select(wlog,{'rows','depth >= 9000 & Vp < 7000'})
%         wlog4=l_select(wlog,{'depths',7500,12000},{'rows','vclay < 0.35'})


%     Set default values for input arguments
param.curves='*';
param.depths=[wlog(1).first,wlog(1).last];
param.rows='';

%       Replace defaults by actual input arguments
param=assign_input(param,varargin);

if iscell(param.depths)
   param.depths=cell2mat(param.depths);
end

lwlog=length(wlog);
if lwlog == 1
   wlog=l_select_no1(wlog,param);

else
   for ii=lwlog:-1:1
       temp(ii)=l_select_no1(wlog(ii),param); %#ok Loop starts with highest index
   end
   wlog=temp;

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function wlog=l_select_no1(wlog,param)

global S4M

ncols=size(wlog.curves,2);

%       Select curves
if strcmp(param.curves,'*')
   cindex=1:ncols;
else
   cindex=curve_indices(wlog,param.curves);
   if isempty(cindex)  ||  any(cindex==0)
      error(' No log curves selected. Abnormal termination.')
   end
   if cindex(1) ~= 1
      cindex=[1,cindex];
   end
end

%       Select rows
param.depths=sort(param.depths);
dindex=find(wlog.curves(:,1) >= param.depths(1)  &  wlog.curves(:,1) <= param.depths(2));
if isempty(dindex)
   error([' Requested depth range (',num2str(param.depths(1)),', ',num2str(param.depths(2)), ...
        ') outside of range of log depths (',num2str(wlog.first),', ',num2str(wlog.last),')'])
end

if ~isempty(param.rows)

%       Find all the words in the logical expression
   words=lower(extract_words(param.rows));
   mnems=wlog.curve_info(ismember(lower(wlog.curve_info(:,1)),words),1);   % Find curve mnemonics in logical expression    
   index=curve_indices(wlog,mnems,0);
   index=unique(index);
   index=index(index > 0); 
   if isempty(index)
       disp([' There are no curve mnemonics in logical expression "',param.rows,'"'])
       error([' Available curve mnemonics are: ',cell2str(wlog.curve_info(:,1))])
   end

%          Create vectors whose names are the curve mnemonics in the logical expression
   for ii=1:length(index)
       eval([lower(char(wlog.curve_info(index(ii),1))),' = wlog.curves(dindex,index(ii));']);
   end

%          Modify expression to be valid for vectors
   expr=strrep(param.rows,'*','.*');
   expr=strrep(expr,'/','./');
   expr=strrep(expr,'^','.^');
   expr=lower(expr);

%          Evaluate modified expression
   try
      rindex=eval(['find(',expr,')']);

   catch  %#ok
      disp([' Expression "',param.rows,'" appears to have errors.'])
      disp(['   Curve mnemonics found in expression: ',cell2str(wlog.curve_info(index,1))])
      disp(['   Curve mnemonics available: ',cell2str(wlog.curve_info(:,1))])
      disp(' Misspelled curve mnemonics would be interpreted as variables.')
      error(' Abnormal termination')
   end

   if isempty(rindex)
       error([' No rows selected by condition "',param.rows,'"'])
   else
       dindex=dindex(rindex);
   end
end

wlog.curve_info=wlog.curve_info(cindex,:);
wlog.curves=wlog.curves(dindex,cindex);
wlog.first=wlog.curves(1,1);
wlog.last=wlog.curves(end,1);
wlog.step=depths2step(wlog.curves(:,1));

%     Add null value if necessary
if isnull(wlog.null)  &&  any(any(isnan(wlog.curves(:,2:end))))
   wlog.null=[];
end
    
%     Select subset of curve types including only those curves that are
%     also in the log
if isfield(wlog,'curve_types') && ~isempty(wlog.curve_types)
   if S4M.case_sensitive
      bool=ismember(wlog.curve_types(:,1),wlog.curve_info(:,1));
   else
      bool=ismember(lower(wlog.curve_types(:,1)),lower(wlog.curve_info(:,1)));
   end
   if ~any(bool)
      wlog=rmfield(wlog,'curve_types');
   else
      wlog.curve_types=wlog.curve_types(bool,:);
   end
end
