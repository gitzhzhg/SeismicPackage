function seisout=s_header_math(seismic,action,expression,units,description)
% Function creates new header using the mathematical expression contained in the
% third input argument. The program expects this expression to contain at least 
% one valid header mnemonic.
%
% Written by E. Rietsch, October 15, 2000
% Last updated: September 5, 2002; handle seismic data without pre-existing headers
%
%         seisout=ds_header_math(seismic,action,expression,units,description)
% INPUT
% seismic     	seismic structure
% action        Defines action to take. Possible values are:
%              'add'      Add header. Gives error message if 
%                         header already exists
%              'add_ne'   Add header with mnemonic mnem. Replaces it if it already exists.
%              'replace'  Replaces header with mnemonic mnem; error if
%                         header does not exist
% expression    expression in MATLAB syntax between headers of seismic. These headers are represented by
%               their mnemonics
% units         units of measurement of the newly created/replaced header
%               if "action" is 'replace' and "units" is empty or not given, 
%                    the units of the replaced header are retained
% description   description of the newly created/replaced header
%               if "action" is 'replace' and "description" is empty or not given, 
%                    the description of the replaced header is retained
%
% OUTPUT
% seisout        input seismic with the additional header appended (including an updated field "header_info")
%
% EXAMPLE
%                seismic=s_data;
%                seismic=s_header_math(seismic,'add','cdp_x=25*cdp+1234','m','CDP X')
%                %    computes the x coordinate of the CDP location. An error message will be 
%                %    issued if "seismic" does not have a header with mnemonic "cdp" or if a 
%                %    header with mnemonic "cdp_x" already exists.
%                s_header(seismic)


%       Find all the words in the expression
words=lower(extract_words(expression));

%       Check if mnemonic of output header (first variable in expression) is already in use
mnem=words{1};

if isfield(seismic,'header_info')
   index=find(ismember(lower(seismic.header_info(:,1)),mnem));
else
   index=[];
end

if ~isempty(index) && strcmpi(action,'add')
   error([' header mnemonic "',mnem,'" already exists in seismic structure "',inputname(1),'"'])
elseif isempty(index) && strcmpi(action,'replace')
   error([' header mnemonic "',mnem,'" does not exist in seismic structure "',inputname(1),'"'])
elseif ~isempty(index) && strcmpi(action,'add_ne')
   action='replace';
end

%       Remove multiple occurrences of a word
words=unique(words);

%       Find all the header mnemonics in "words" and assign header values to 
%       variables with those names
if isfield(seismic,'header_info')
   idx=find(ismember(lower(seismic.header_info(:,1)),words));
else
   idx=[];
end

if isempty(idx) && sum(ismember(words,'trace_no')) == 0
   disp([' No header mnemonics found in expression "',expression,'"'])
   disp(' header mnemonics available')
   disp(seismic.header_info(:,1)')
   error(' Abnormal termination')
end
for ii=1:length(idx)
   eval([lower(char(seismic.header_info(idx(ii),1))),' = seismic.headers(idx(ii),:);']);
end

if any(ismember(words,'trace_no'))
   trace_no=1:size(seismic.traces,2); %#ok Used in "eval"
end


%       Modify expression to be valid for vectors
expr=strrep(lower(expression),'*','.*');
expr=strrep(expr,'/','./');
expr=strrep(expr,'^','.^');

%       Evaluate modified expression
     try
eval([expr,';'])

    catch
disp([' Expression "',expression,'" appears to have errors'])
disp(' header mnemonics found in expression:')
disp(seismic.header_info(idx,1)')
disp(' header mnemonics available')
disp(seismic.header_info(:,1)')
disp(' Misspelled header mnemonics would be interpreted as variables')
eval(expr);
error(' Abnormal termination')
    end

%       Add new header or replace existing one
switch action
               case {'add','add_ne'}
if isfield(seismic,'headers')
  seisout.headers=[seismic.headers;eval(mnem)];
  seisout.header_info=[seismic.header_info;{mnem,units,description}];
else
  seisout.headers=eval(mnem);
  seisout.header_info={mnem,units,description};
end

if ~isfield(seismic,'header_null')	%  Add header null field if needed.		
  idx=sum(isnan(seisout.headers(end,:)));
  if idx > 0
     seisout.header_null=NaN;
  end
end

               case 'replace'
seisout.headers=seismic.headers;
seisout.header_info=seismic.header_info;
seisout.headers(index,:)=eval(mnem);
if ~exist('units','var') || isempty(units)
  units=s_gu(seismic,mnem);
end
if ~exist('description','var') || isempty(description)
  description=s_gd(seismic,mnem);
end
seisout.header_info(index,:)={mnem,units,description};

if ~isfield(seismic,'header_null')	%  Add header null field if needed.		
  idx=sum(isnan(seisout.headers(index,:)));
  if idx > 0
     seisout.header_null=NaN;
  end
end
                otherwise
error(' Unknown value for input parameter "action"')
                
end		% End of switch block

%       Copy rest of fields
seisout=copy_fields(seismic,seisout);

% seisout=s_compact2new(seisout);

