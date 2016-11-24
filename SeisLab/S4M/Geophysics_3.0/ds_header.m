function dataset=ds_header(dataset,action,mnem,values,units,description)
% DS_HEADER: Function manipulates/displays header(s) of a seismic or a 
% pseudo-well dataset.
% The second input argument, "action", determines what to do.
% See also: ds_add_header
%
% Written by: E. Rietsch: Date March 16, 2000
% Last updated: December 11, 2007: Adapt for use for seismic and for pseudo-well
%                                 datasets
%
%           dataset=ds_header(dataset,action,mnem,values,units,description)
%           dataset=ds_header(dataset,action,mnem)   for action='delete','keep',
%                                                    'delete_ne','keep_ne','list'
%           dataset=ds_header(dataset)               assumes action 'list'         
% INPUT
% dataset  Seismic or pseudo-well dataset.
%          If this is the only input argument, "action" is set to 'list' and "mnem' is
%          set to '*'. The function prints a list of all header values as describend 
%          below. 
% action   Defines action to take. Possible values are:
%          'add'      Add header with mnemonic "mnem" (third input argument). 
%                     Issues error message if the header already exists
%          'add_ne'   Add header with mnemonic "mnem". Replaces it if it already exists.
%          'replace'  Replaces header with mnemonic "mnem"; error if
%                     header does not exist
%          'delete'   Delete header(s) with mnemonic(s) "mnem"; 
%                     error if header does not exist 
%          'delete_ne' Delete header(s) with mnemonic(s) "mnem" if the header is present
%                     no error if one or more of the header(s) specified does not exist 
%          'keep'     Keep header(s) with mnemonic(s) "mnem" and deletes all others;
%                     error if one or more of the header(s) do not exist 
%          'keep_ne'  Keep header(s) with mnemonic(s) "mnem" if it is (they are) present
%                     and deletes all others; no error if any or all headers specified 
%                     are not present
%          'rename'   Rename header mnemonic, keep everything else the same
%          'list'     Print short list: for specified header mnemonic(s) it lists minimum 
%                     and maximum value, smallest and greatest trace-to-trace change,
%                     units of measurement, and header description
%          
%          The other input parameters depend on the parameter "action"
%
%                   CASE action = 'add', 'add_ne', or 'replace'
% mnem     String with header mnemonic
% values   Header values; if only one value is given the header is assumed 
%          to be constant
% units    Units of measurement for curve values (optional if 'action' is 
%          'replace')
% description  Description of curve mnemonic (optional if 'action' is 'replace')
%          If "action" is 'replace' "units" and "description" need not be given; 
%          in this case the original "units" and "description" are retained.
%
%                  CASE action = 'delete', delete_ne', 'keep', 'keep_ne', or 'list'
% mnem     Header mnemonic or cells array with header mnemonics
%          '*' means all headers
%
%                  CASE action = 'rename'
% mnem     Cell array consisting of two strings: the old and 
%          the new name of the header
%
% OUTPUT
% dataset  "Updated" seismic or pseudo-well structure (no output if action == 'list')
%
% EXAMPLE
%          seismic=s_data;
%          seismic=ds_header(seismic,'add','test',11,'ft','Test header');
%          ds_header(seismic)

% UPDATE HISTORY
%          December 2, 2006: Handle single/double precision
%          October 17, 2007: Use original units of measurement and description 
%                            if they have not been specified in the input

global S4M


if nargin < 1
   error('At least one input argument (seismic dataset or pseudo-well structure) required.')
end

if ~istype(dataset,'seismic')  &&  ~istype(dataset,'pseudo-wells')
   error('The first input argument must be a seismic dataset or a pseudo-well structure.')
end

if nargin == 1
   action='list';
   mnem={'*'};
else
   action=lower(action);
   if ~iscell(mnem)
      mnem=tokens(mnem,',');
   end
   if ~S4M.case_sensitive
      mnem=lower(mnem);
   end
end

%       Determine precision of input dataset
if isfield(dataset,'traces')
   precision=class(dataset.traces);
else
   precision=class(dataset.(dataset.panel_info{1,1}));
end

if ~isfield(dataset,'headers') 
   if ~strcmp(action,'add') && ~strcmp(action,'add_ne')
      disp('Dataset has no headers.')
      if nargout == 0
         clear dataset; 
      end
      return
   else
      nh=0;
      if strcmpi(precision,'single')
         values=single(values);
      else
         values=double(values);
      end
      mh=dsSize(dataset,2);
   end
else
   [nh,mh]=size(dataset.headers);
end

if isfield(dataset,'history')
   dataset=s_history(dataset,'append',[char(action),': ',cell2str(mnem,', ')]);
end


lmnem=length(mnem);

if isfield(dataset,'header_info')
   if S4M.case_sensitive   
      existing_mnems=dataset.header_info(:,1);
   else
      existing_mnems=lower(dataset.header_info(:,1));
   end
   if lmnem == 1  &&  strcmp(char(mnem),'*')  && ...
      ~strcmp(action,'add') && ~strcmp(action,'add_ne') && ~strcmp(action,'replace')
      mnem=existing_mnems;
   end
else
   existing_mnems=[];
end

switch action
               case {'add','add_ne','replace'}

lv=length(values);
if lv == 1
   val=values; 
elseif lv == mh
   val=reshape(values,1,mh);
else
   if istype(dataset,'seismic')
      error(['Input argument "values" must be a constant or a vector ', ...
         'whose length is equal to the number of traces in the seismic dataset.'])
    else
      error(['Input argument "values" must be a constant or a vector ', ...
         'whose length is equal to the number of pseudo-wells.'])
    end
end

if ~isfield(dataset,'header_info')
   idx=[];
else
   [idx,ierr]=header_index1(dataset,mnem);   %#ok Second output argument required to prevent abort in case of error
end

if ~isempty(idx)
   if strcmpi(action,'add')
      error(['The header mnemonic ',char(mnem),' already exists in the header.'])
   else
      dataset.headers(idx,:)=val;
      if nargin > 4
         dataset.header_info{idx,2}=units;
         if nargin > 5
            dataset.header_info{idx,3}=description;
         end
      end
   end
else
   if strcmpi(action,'replace')
      error(['The header mnemonic ',char(mnem),' does not exist in field "header_info".'])
   else
      idx=nh+1;
      dataset.header_info(idx,1)=mnem;
      dataset.headers(idx,1:mh)=val;
      try
         dataset.header_info{idx,2}=units;
         dataset.header_info{idx,3}=description;
      catch
         % Keep original units and description
      end
   end      
end

              case {'delete','delete_ne'}
idx=find(ismember(existing_mnems,mnem));
if lmnem ~= length(idx) && strcmp(action,'delete')
   for ii=1:lmnem
      if ~ismember(existing_mnems,mnem(ii))
         disp([' Header "',mnem{ii},'" is not present.'])
      end
   end
   error('Abnormal termination')
end

if length(idx) == length(existing_mnems)
   dataset=rmfield(dataset,{'headers','header_info'});
else
   dataset.headers(idx,:)=[];
   dataset.header_info(idx,:)=[];
end 

              case {'keep','keep_ne'}
idx=find(~ismember(existing_mnems,mnem));
if lmnem ~= nh-length(idx) && strcmp(action,'keep')
  for ii=1:lmnem
    if ~ismember(existing_mnems,mnem(ii))
      disp(['Header ',char(mnem(ii)),' not present'])
    end
  end
  error(' Abnormal termination')
end
if length(idx) == nh
  dataset=rmfield(dataset,{'headers','header_info'});
else
  dataset.headers(idx,:)=[];
  dataset.header_info(idx,:)=[];
end 

              case 'rename'
if lmnem ~= 2
  error(['Parameter "mnem" must be a cell array with two strings' , ... 
        '(old header mnemonic and new header mnemonic)'])
elseif ismember(existing_mnems,mnem(2))
  error(['New header mnemonic ',char(mnem(2)),' already exists in the header.'])
else
  idx=find(ismember(existing_mnems,mnem(1)));
  if isempty(idx)
    error(['Header ',char(mnem(1)),' does not exist'])
  else
    dataset.header_info(idx,1)=mnem(2);
  end
end

              case 'list'

idx=find(ismember(existing_mnems,mnem)); 

if isempty(idx)
  error([' Header(s) "',cell2str(mnem,'", "'),'" do(es) not exist'])
end

mnems=char('MNEMONIC',dataset.header_info{idx,1});
descr=char('DESCRIPTION',dataset.header_info{idx,3});
units=char('UNITS',dataset.header_info{idx,2});
hmin=min(dataset.headers(idx,:),[],2);
hmax=max(dataset.headers(idx,:),[],2);
dh=diff(dataset.headers(idx,:),[],2);
dhmin=min(dh,[],2);
dhmax=max(dh,[],2);
spaces(1:length(hmax)+1)=' ';

if isempty(dhmin)     % handle case of only one input trace (no trace-to-trace change)
   dhmin=' n/a';
   sdhmin=char('MIN_STEP',dhmin(ones(length(hmin),1),:));
   sdhmax=char('MAX_STEP',dhmin(ones(length(hmin),1),:));
else
   sdhmin=char('MIN_STEP',num2str(dhmin));
   sdhmax=char('MAX_STEP',num2str(dhmax));
end

shmin=char('MIN_VAL',num2str(hmin));
shmax=char('MAX_VAL',num2str(hmax));
disp(['Headers of data set "',dataset.name,'":'])
disp([mnems,spaces',shmin,spaces',spaces',shmax,spaces', ...
      spaces',sdhmin,spaces',spaces',sdhmax,spaces',units,spaces',descr]);

%       Write message if one or more of the headers specified were not found
if lmnem ~= length(idx)    
  for ii=1:lmnem
    if ~ismember(existing_mnems,mnem(ii))
      disp([char(mnem(ii)),' is not present.'])
    end
  end
end


              otherwise
error(['Action ',action,' not defined.'])

end	      % End of switch block

if nargout == 0
   clear dataset;
end
