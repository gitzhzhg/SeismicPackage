function wlog=l_curve_math(wlog,action,expression,units,description)
% Function creates new log curve using the mathematical expression contained in the
% third input argument.
%
% Written by: E. Rietsch: September 2, 2000
% Last updated: September 29, 2003: use curve_index1 to get index of curve; 
%                           input log and output log have the same name
%
%    	     wlog=l_curve_math(wlog,action,expression,units,description)
% INPUT
% wlog       log structure
% action     defines action to take. Possible values are:
%            'add'      Add curve. Gives error message if curve mnemonic already exists
%            'add_ne'   Add curve with mnemonic mnem. Replaces it if it already exists.
%            'replace'  Replaces curve with mnemonic mnem; error if
%                       curve mnemonic does not exist
% expression  expression in MATLAB syntax between curves of wlog. These curves are  
%            represented by their mnemonics
% units      units of measurement of the newly created/replaced curve
%            if 'action' is 'replace' and "units" is empty or not given, 
%                  the units of the replaced curve are retained
% description  description of the newly created/replaced curve
%            if 'action' is 'replace' and "description" is empty or not given, 
%                  the description of the replaced curve is retained
%
% OUTPUT
% wlog       input log with the additional curve appended (including an updated field "curve_info")
%
% EXAMPLE
%            wlog=l_data;
%            wlog=l_curve_math(wlog,'add','vel=1.e6/DTp+100','ft/s','Velocity')
%            %     computes the velocity from the sonic log. An error message will be 
%            %     issued if "wlog" does not have a curve with mnemonic "DT" or if a 
%            %     curve with mnemonic "vel" already exists.
%            l_plot1(wlog,{'curves','Vp','vel'})


%       Find all the words in the expression
words=lower(extract_words(expression));

%       Check if mnemonic of output curve (first variable in expression) is already in use
mnem=words{1};
[index,dummy]=curve_index1(wlog,mnem);   %#ok Second argument used to 
                                         % indicate error handling outside
if ~isempty(index) && strcmpi(action,'add')
   error([' Curve mnemonic "',mnem,'" already exists in log structure "',inputname(1),'"'])
elseif isempty(index) && strcmpi(action,'replace')
   error([' Curve mnemonic "',mnem,'" does not exist in log structure "',inputname(1),'"'])
elseif ~isempty(index) && strcmpi(action,'add_ne')
   action='replace';
end


%       Remove multiple occurrences of a word
words=unique(words);

%       Find all the curve mnemonics in "words" and assign curve values to variables with those names 
idx=find(ismember(lower(wlog.curve_info(:,1)),words));
if isempty(idx)
   disp([' No curve mnemonics found in expression "',expression,'"'])
   disp(' Curve mnemonics available')
   disp(wlog.curve_info(:,1)')
   error(' Abnormal termination')
else
  for ii=1:length(idx)
    eval([lower(char(wlog.curve_info(idx(ii),1))),' = wlog.curves(:,idx(ii));']);
  end
end

%       Modify expression to be valid for vectors
expr=strrep(lower(expression),'.*','*');
expr=strrep(expr,'*','.*');
expr=strrep(expr,'./','/');
expr=strrep(expr,'/','./');
expr=strrep(expr,'.^','^');
expr=strrep(expr,'^','.^');

%       Evaluate modified expression
     try
eval([expr,';'])

    catch
disp([' Expression "',expression,'" appears to have errors'])
disp(' Curve mnemonics found in expression:')
disp(wlog.curve_info(idx,1)')
disp(' Curve mnemonics available:')
disp(wlog.curve_info(:,1)')
disp(' Misspelled curve mnemonics would be interpreted as variables')
eval(expr);
error(' Abnormal termination')
    end

%       Add new curve or replace existing one
switch action
               case {'add','add_ne'}

wlog.curves=[wlog.curves,eval(mnem)];
wlog.curve_info=[wlog.curve_info;{mnem,units,description}];

if ~isfield(wlog,'null')	%  Add null field if needed.		
   idx=sum(isnan(wlog.curves(:,end)));
   if idx > 0
      wlog.null=NaN;
   end
end

               case 'replace'

wlog.curves(:,index)=eval(mnem);
if ~exist('units','var') || isempty(units)
   units=l_gu(wlog,mnem);
end
if ~exist('description','var') || isempty(description)
   description=l_gd(wlog,mnem);
end
wlog.curve_info(index,:)={mnem,units,description};
       
if ~isfield(wlog,'null')	%  Add null field if needed.		
   idx=sum(isnan(wlog.curves(:,index)));
   if idx > 0
      wlog.null=NaN;
   end
end

                otherwise
error(' Unknown value for input parameter "action".')
                
end

if ~isempty(index)  &&  index == 1	% Change depth column
   index=find(isnan(wlog.curves(:,1)));
   if ~isempty(index)
      wlog.curves(index,:)=[];
   end
   wlog.first=wlog.curves(1,1);
   wlog.last=wlog.curves(end,1);
   wlog.step=depths2step(wlog.curves(:,1));
end
