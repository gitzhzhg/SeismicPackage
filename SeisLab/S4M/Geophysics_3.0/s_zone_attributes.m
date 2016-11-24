function seismic=s_zone_attributes(seismic,varargin)
% Compute attributes such as minimum, maximum, RMS amplitude in a trace-dependent
% interval/zone and either print them to the screen or store them in like-named 
% header(s); specifically, if no output data set is provided, information about
% these attributes is printed via the 'list' (short list) option of "ds_header".
% See also: s_attributes
%
% Written by: E. Rietsch: December 17, 2005
% Last updated: August 1, 2006: bug fixes
%
%              seismic=s_zone_attributes(seismic,varargin)            
% INPUT
% seismic    Seismic structure;          
% varargin   one or more cell arrays; the first element of each cell array is a keyword,
%            the other elements are parameters. Presently, keywords are:
%     'action'  Defines action to take. Possible values are:
%            'add'      Add header with mnemonic "attribute". Gives error message if 
%                       header already exists 
%            'add_ne'   Add header with mnemonic "attribute". Replaces it if it already exists.
%            'Default: {'action','add_ne'}
%     'attributes'   Defines the attributes to compute. Possible attributes
%                    are (not case sensitive):
%            'aaa'      Compute the average absolute amplitude of each trace and store it 
%                         in header "aaa" (same as "amean")
%            'amax'     Compute the maximum absolute value of each trace and store 
%                         it in header "amax"
%            'amean'    Compute the mean of the absolute value of each trace
%                         and store it in header "amean"
%            'amedian'  Compute the median of the absolute value of each trace
%                         and store it in header "amedian"
%            'amin'     Compute the minimum absolute value of each trace and store 
%                         it in header "amin"
%            'l2norm'   Compute the L2 norm of each trace and store it in 
%                         header "l2norm"
%            'max'      Compute the maximum value of each trace and store it 
%                         in header "max"
%            'mean'     Compute the mean value of each trace and stores it in 
%                         header "mean"
%            'median'   Compute the median value of each trace and store it in 
%                         header "median"
%            'min'      Compute the minimum value of each trace and store it in 
%                         header "min"
%            'minabs'   Compute the absolute value of the minimum of each trace 
%                          and store it in header "minabs"
%            'rms'      Compute the rms value of each trace and store it in
%                          header "rms"
%            Default: {'attributes','aaa','amax','amean', ... ,'rms'}  (all attributes)
%    'zone'  Top and base of the zone in which the attribute(s) should be computed.
%            {'zone',top,base}. Here "top" can be scalar representing the 
%            top of the zone or a header which contains the time of the top.
%            Likewise, "base" can be scalar representing the base of the
%            zone or a header which contains the time of the base.
%            Default: {'zone',seismic.first,seismic.last}
%    'min_thick'      minimum thickness of interval (for thicknesses < 'min_thick" the
%                     attribute is set to NaN
%            Default: {'min_thick',seismic.step}
% OUTPUT
% seismic    "Updated" seismic structure; if no output argument is given the function
%            outputs a summary of the attributes (via "ds_header")
%
% EXAMPLE
%            seismic=s_data;
%            seismic=s_zone_attributes(seismic,{'action','add'},{'attributes','rms','amean'});
%            ds_header(seismic)

%       Set defaults
param.action='add_ne';
param.zone=[];
param.attributes={'aaa','amax','amean','amedian','amin','l2norm','max', ...
        'mean','median','min','minabs','rms',};
param.min_thick=seismic.step;

%       Incorporate input data
param=assign_input(param,varargin);

if nargin < 1
   error('At least one input argument (seismic structure) required')
end

if ~istype(seismic,'seismic')
   error('The first input argument must be a seismic data set')
end

if ~isfield(seismic,'headers')
   nh=0;
else
   nh=size(seismic.headers,1);
end

if ischar(param.attributes)
   param.attributes={param.attributes};
end


[nsamp,ntr]=size(seismic.traces);

if isempty(param.zone)	% Compute attributes for the whole trace
   top=seismic.first*ones(1,ntr);
   base=seismic.last*ones(1,ntr);

else	% Compute attributes for a zone only
   if ischar(param.zone{1})	% Get top from header
      top=s_gh(seismic,param.zone{1});
   elseif isnumeric(param.zone{1}) && length(param.zone{1}) == 1 
      top=param.zone{1}*ones(1,ntr);
   else
      error('Incorrect definition of top of zone; must be a constant or a header mnemonic.')
   end

   if ischar(param.zone{2})	% Get base from header
      base=s_gh(seismic,param.zone{2});
   elseif isnumeric(param.zone{2}) && length(param.zone{2}) == 1   
      base=param.zone{2}*ones(1,ntr);
   else
      error('Incorrect definition of base of zone; must be a constant or a header mnemonic.')
   end
end


topindex=round((top-seismic.first)/seismic.step)+1;
baseindex=round((base-seismic.first)/seismic.step)+1;
idx4null=isnan(topindex)  |  isnan(baseindex)  |  ...
         (baseindex-topindex < param.min_thick/seismic.step)  | ...
	 topindex < 1  |  baseindex > nsamp;
idx2use=find(~idx4null);
if isempty(idx2use)
   error('No valid zone defined for any of the traces.')
end

htext=cell(length(param.attributes)+1,1);
htext{1}=[param.action,':'];                % Prepare text for history field


for jj=1:length(param.attributes)
   attribute=lower(param.attributes{jj});

%     Check if a header with the name "attribute" already exists and determine row of header
%     and header_info to store new data
   if ~isfield(seismic,'header_info')
      idx=[];
   else
      idx=find(ismember(lower(seismic.header_info(:,1)),attribute));
   end
   if ~isempty(idx)
      if strcmpi(param.action,'add')
         error(['The header mnemonic ',attribute,' already exists in the header.'])
      end
   else
      idx=nh+1;
   end

   %%    Compute teh requested attributes
   switch attribute
   case 'max'

      for ii=idx2use
         seismic.headers(idx,ii)=max(seismic.traces(topindex(ii):baseindex(ii),ii));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Maximum of trace'};

   case 'amax'
      for ii=1:ntr
         seismic.headers(idx,ii)=max(abs(seismic.traces(topindex(ii):baseindex(ii),ii)));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Maximum of absolute values of trace'};

   case 'min'
      for ii=1:ntr
         seismic.headers(idx,ii)=min(seismic.traces(topindex(ii):baseindex(ii),ii));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Minimum of trace'};

   case 'amin'
      for ii=1:ntr
         seismic.headers(idx,ii)=abs(min(seismic.traces(topindex(ii):baseindex(ii),ii)));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Absolute value of trace minimum'};

   case 'minabs'
      for ii=1:ntr
         seismic.headers(idx,ii)=min(abs(seismic.traces(topindex(ii):baseindex(ii),ii)));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Minimum of absolute values of trace'};

   case 'mean'
      for ii=1:ntr
         seismic.headers(idx,ii)=mean(seismic.traces(topindex(ii):baseindex(ii),ii));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Mean of trace'};

   case {'amean','aaa'}
      for ii=idx2use
         seismic.headers(idx,ii)=mean(abs(seismic.traces(topindex(ii):baseindex(ii),ii)));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Mean of absolute values of trace'};

   case 'median'
      for ii=1:ntr
         seismic.headers(idx,ii)=median(seismic.traces(topindex(ii):baseindex(ii),ii));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Median of trace'};

   case 'l2norm'
      temp=zeros(1,ntr);
      for ii=1:ntr
         temp(ii)=norm(seismic.traces(topindex(ii):baseindex(ii),ii));
      end
      seismic.headers(idx,:)=temp;
      seismic.header_info(idx,1:3)={attribute,'n/a','L2 norm of trace'};

   case 'amedian'
      for ii=1:ntr
         seismic.headers(idx,ii)=median(abs(seismic.traces(topindex(ii):baseindex(ii),ii)));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','Median of absolute values of trace'};

   case 'rms'
      for ii=1:ntr
         seismic.headers(idx,ii)=sqrt(sum(seismic.traces(topindex(ii):baseindex(ii),ii).^2) ...
          /(baseindex(ii)-topindex(ii)+1));
      end
      seismic.header_info(idx,1:3)={attribute,'n/a','RMS amplitude of trace'};

   otherwise
      error(['Attribute ',attribute,' not (yet?) defined'])

   end	% End of SWITCH block

%% Post-processing in FOR block
   idx4null=find(idx4null);
   if ~isempty(idx4null)
      seismic.headers(idx,idx4null)=NaN;
   end


   nh=size(seismic.headers,1);    % Update number of header mnemonics
   htext{jj+1}=attribute;         % Update text for history file

end     % End of FOR block

%%  If no output argument is specified
if nargout == 0  
   if isempty(inputname(1))
      unknown=seismic;
      ds_header(unknown,'list',param.attributes)
   else
      eval([inputname(1),'=seismic;']);
      ds_header(eval(inputname(1)),'list',param.attributes);
   end
   clear seismic

else

%    Append history field
   if isfield(seismic,'history')
     seismic=s_history(seismic,'append',cell2str(htext,' '));
   end
end   
