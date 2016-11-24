function seisout=s_select(seismic,varargin)
% Function extracts subset of seismic input traces. An option also allows changing the time
% range of the output data. If seisout.first < seismic.first and/or seisout.last > seismic.last
% then the trace samples undefined by the input data are set to the null value.
% See also: "s_trace_numbers"
%
% Written by: E. Rietsch: April 15, 2000
% Last updated: April 17, 2008: Input can be vectors of seismic datasets
%
%          seisout=s_select(seismic,varargin)
% INPUT
% seismic  seismic structure
% varargin one or more cell arrays; the first element of each cell array is a
%          keyword string, the following arguments contains a parameter(s). 
%          Accepted keywords are:
%   times  For a two-element cell array the second element is a vector 
%          of time values.For a three-element cell array the second and 
%          third element contain the time of the first sample and the 
%          last sample respectively. 
%          Hence, {'times',first,last} and {'times',[first,'last]}
%          generally do different things; the former selects all the
%          samples between time "first" and time "last"; the latter 
%          selects the two samples associated with times "first" and
%          time "last"
%          Default: {'times',[]}   the time range of the datset is retained                
%                 which is equivalent to {'times',seismic.first,seismic.last}
%    traces  The second element can be a vector of trace numbers or it can be a string.
%          If it is a string it can be a header mnemonic or it can contain a logical 
%          expression involving header values to include.
%          A "pseudo-header" 'trace_no' can also be used.
%          If the second element is a string containing a header mnemonic there must 
%          be a third element containing a vector of values
%          Default:  {'traces',[]} which is equivalent to {'traces',1:ntr}
%                                  where ntr denotes the number of traces in the 
%                                  input data set (ntr = size(seismic.traces,2))
%                 Headers are selected as found and are not sorted.
%    null  Null value to assign to seismic data values for times < seismic.first 
%                and/or times > seismic.last. 
%          Default: "null=0" unless seismic has null values; then "null=NaN".
%             
%          Examples:
%          s_select(seismic,{'traces',1:10})          % Select traces 1 to 10
%          s_select(seismic,{'traces','trace_no',1:10})  % Same as above
%          s_select(seismic,{'traces','trace_no',1,10})  % Same as above
%          s_select(seismic,{'times',0,1000},{'traces','cdp',100:10:200})
%
%                          % Select traces with CDP numbers 100 to 200 with increment 10
%          s_select(seismic,{'times',seismic.first:2*seismic.step:seismic.last})
%                          % Select traces with CDP's 100 and above
%          s_select(seismic,{'traces','cdp',100,inf}) 
%                          % This is equivalent to the previous function call
%          s_select(seismic,{'traces','cdp >= 100'})   
%                          % The following is an even more complex function call
%          s_select(seismic,{'traces','iline_no > 1000 & iline_no < 1100 & xline_no == 1000'})
%                                             
% OUTPUT
% seisout  seismic output structure
%
% EXAMPLES
%          seismic=s_data;
%          seismic1=s_select(seismic,{'traces','CDP > 105'});
%          s_header(seismic1)
%          seismic2=s_select(seismic,{'traces','mod(cdp,2) == 0 & cdp <= 108'});
%          s_header(seismic2)

% UPDATE HISTORY
%          April 16, 2007: Output has precision of input

global S4M

if ~istype(seismic(1),'seismic')
   error('The first input argument, "seismic", must be a seismic data set.')
end

      
%%       Set defaults for input parameters
if isnull(seismic)
   param.null=NaN;
else
   param.null=0;
end
param.times=[];
param.traces=[];

%       Replace defaults by the actual input arguments
param=assign_input(param,varargin,'s_select');


%%     Check if input is a datset vector
lseismic=length(seismic);
if lseismic > 1
   for ii=lseismic:-1:1 
      seisout(ii)=s_select(seismic(ii),{'null',param.null}, ...
         {'times',param.times},{'traces',param.traces});     %#ok  Loop starts with largest index
   end   
   return
end


%%     Input is not a dataset vector
seisout=seismic;
precision=class(seismic.traces);

[nsamp,ntr]=size(seismic.traces);

if isempty(param.times)
   param.times={seismic.first,seismic.last};
end

%      Determine index for times
if iscell(param.times)
   ta=param.times{1};
   te=param.times{2};
   if te < ta
      error(['First time (',num2str(ta),' greater than last time (',num2str(te),')'])
   end
   nta=round((ta-seismic.first)/seismic.step);
   ta=nta*seismic.step+seismic.first;
   nte=round((te-seismic.first)/seismic.step);
   te=nte*seismic.step+seismic.first;
   t_index=nta+1:nte+1;
   seisout.step=seismic.step;

else
   t_index=round((param.times-seismic.first)/seismic.step)+1;
   ta=(min(t_index)-1)*seismic.step+seismic.first;
   te=(max(t_index)-1)*seismic.step+seismic.first;
   if length(t_index) > 1
      dt=diff(t_index);
      if max(dt) ~= min(dt)
         error('Nonuniform time samples selected')
      else
         seisout.step=dt(1)*seismic.step;
      end
   else
      seisout.step=seismic.step;
   end
end
seisout.first=ta;
seisout.last=te;

%       Find index for traces
if isempty(param.traces)
   h_index=1:ntr;

else
   if ~iscell(param.traces)
      param.traces={param.traces};
   elseif iscell(param.traces{1})
      param.traces=param.traces{1};
   end

   if ~ischar(param.traces{1})            % Traces given explicitely
      if length(param.traces) > 1
         h_index=max([1,param.traces{1}]):min([size(seismic.traces,2),param.traces{2}]);
      else
         h_index=param.traces{1};
      end
      if max(h_index) > ntr
         error([' Only ',num2str(ntr),' trace(s) available, but largest trace number selected: ',num2str(max(h_index))])
      end

   elseif length(param.traces) > 1        % Traces defined via header
      header=lower(param.traces{1});
      if isfield(seismic,'header_info')
         idx=ismember_ordered(lower(seismic.header_info(:,1)),header);
      else
      idx=[];
      end
      if isempty(idx)
         if strcmp(header,'trace_no')
            header_vals=1:ntr;
         else
            error(['Header "',header,'" is not present in the seismic input dataset.'])
         end
      else
         header_vals=seismic.headers(idx,:);
      end

      if length(param.traces) == 2   % Range of header values specified
         hidx=param.traces{2};
         h_index=ismember_ordered(header_vals,hidx);
         if isempty(h_index)
            error(['Header "',header,'" has no values matching the ones requested.'])
         end

      else                        % First and last header value specified
         ha=param.traces{2}; 
         he=param.traces{3};
         h_index=find(header_vals >= ha & header_vals <= he);
         if isempty(h_index)
            error(['Header "',header,'" has no values within range specified (',num2str([ha,he]),')'])
         end
      end

   else                           % Traces defined via logical a expression
      h_index=find_trace_index(seismic,param.traces{1});
   end
end

%     Copy seismic data
if ta < seismic.first || te > seismic.last
   if param.null == 0
      seisout.traces=zeros(length(t_index),length(h_index),precision);
   else
      seisout.null=param.null;
      seisout.traces=param.null*ones(length(t_index),length(h_index),precision);
   end
   idxi=find(ismember(1:nsamp,t_index));
   bool=ismember(t_index,1:nsamp);
   if isempty(idxi)
      error([' Time range specified (',num2str(ta),' - ',num2str(te), ...
          ') does not include time range of input data (', ...
          num2str(seismic.first),' - ',num2str(seismic.last),')'])
   end
   seisout.traces(bool,:,:)=seismic.traces(idxi,h_index,:);
else
   seisout.traces=seismic.traces(t_index,h_index,:);
end

%     Copy headers
if isfield(seismic,'headers')
   seisout.headers=seismic.headers(:,h_index);
end

% 	Check for existence of null values
bool=any(isnan(seisout.traces(:)));
if bool
   seisout.null=NaN;
else
   seisout.null=[];
end

%    Append history field
if S4M.history && isfield(seismic,'history')
   htext=['Times: ',num2str(seisout.first),' - ',num2str(seisout.last),' ',seisout.units,', ', ...
          num2str(length(h_index)),' traces'];
   seisout=s_history(seisout,'append',htext);
end

if isempty(seisout.traces)
   alert(' No traces selected. Empty trace matrix output.')
else
   if strcmp(precision,'single')
      seisout.null=single(seisout.null);
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function index=find_trace_index(seismic,expression)
% Function finds index of traces whose header values match a logical expression
% INPUT
% seismic       seismic data set
% expression    logical expression involving header values
% OUTPUT
% index         index of trace numbers (traces "seismic.traces(:,h_index)" are selected)

global S4M

words=symvar(expression);   % Get all the variable in the expression

if ~S4M.case_sensitive
   words=lower(words);
   headers=lower(seismic.header_info(:,1));
else
   headers=seismic.header_info(:,1);
end

ik=0;
for ii=1:length(words)
   temp=find(ismember(headers,words(ii))); %#ok Variable "temp" is used in "eval"

   if isempty(temp)
      ik=ik+1;
      if strcmp(words(ik),'trace_no')   % Trace_no is used in expression "expression"
         trace_no=1:size(seismic.headers,2);   %#ok Required further below in "eval" 
      end
   else
      ik=ik+1;
      eval([words{ik},'=seismic.headers(temp,:);']);
   end
end
if ik == 0
   disp([' No header mnemonics in expression "',expression,'".'])
   disp(' Available header mnemonics (besides "trace_no") are:')
   disp(cell2str(seismic.header_info(:,1),', '));
   error(' Abnormal termination')
end

try
   index=eval(['find(',lower(expression),')']);

catch %#ok
   disp([' The argument of keyword "traces" (',expression,')'])
   disp(' is probably not a valid logical expression or a header mnemonic has been misspelled.')
   error(' Abnormal termination')

end
