function [index,ierr]=s_trace_numbers(seismic,string,varargin)
% Function outputs trace numbers of seismic input traces based on 
% selected header values.
% See also: "s_select"
%
% Written by: E. Rietsch: April 15, 2000
% Last updated: July 23, Revamped code
%
%          [index,ierr]=s_trace_numbers(seismic,string,varargin)
% INPUT
% seismic  seismic structure
% string   the string it can be a header mnemonic or it can contain a 
%          logical expression involving header mnemonics and header values.
%          A "pseudo-header" 'trace_no' can also be used.
% varargin  one or two arguments if "string" is a header mnemonic.
%          If there is only one argument it is a vector of header values 
%          e.g. s_trace_numbers(seismic,'cdp',100:200)
%          If there are two arguments they denote the lower and upper limit of
%          header values 
%          e.g. s_trace_numbers(seismic,'cdp',100,200)
% OUTPUT
% index    column vector with indices of trace numbers
% ierr     error code
%          no error:                                         ierr = 0
%          Header has no values matching the ones requested: ierr = 1
%          Header has no values within range specified:      ierr = 2
%          No header mnemonics found in expression:          ierr = 3
%          Not a valid logical expression:                   ierr = 4
%          If the second output argument is not supplied and an error occurs 
%          the function aborts with the appropriate error message
%
% EXAMPLES OF USAGE       
%          index=s_trace_numbers(seismic,'offset',[100:100:2000])
%          index=s_trace_numbers(seismic, ...
%                'iline_no > 1000 && iline_no < 1100 && xline_no == 1000')
% EXAMPLES
%          seismic=s_data;
%          index1=s_trace_numbers(seismic,'ismember(cdp,[106,107,108])')
%          index1a=s_trace_numbers(seismic,'cdp',106,108)% This is equivalent to 
%                                                        % the previous command
%          index1b=s_trace_numbers(seismic,'cdp',106:108)% This is equivalent to 
%                                                        % the previous command
%
%          index1x=s_trace_numbers(seismic,'cdp',[106,106.5,108])
%          index1y=s_trace_numbers(seismic,'cdp',[107,106,108])
%
%          index2=s_trace_numbers(seismic,'cdp',105,inf)
%          index2a=s_trace_numbers(seismic,'cdp >= 105') % This is equivalent to 
%                                                        % the previous command

% UPDATE HISTORY
%           September 13, 2004: Second output argument


if ~istype(seismic,'seismic')
   error('First input argument "seismic" must be a structure.')
end
ierr=0;

if nargin < 2
   error('There must be at least 2 input arguments')
end

if nargin == 2               % Traces defined via logical expression
   [index,ierr]=find_trace_index(seismic,string);

elseif nargin == 3           % Header values given explicitly
   header_vals=s_gh(seismic,string);

   hidx=varargin{1};
   index=ismember_ordered(header_vals,hidx);
   index=reshape(index,1,[]);
   if isempty(index)
         if nargout <= 1
            disp(' Requested header values:')
            disp(hidx)
            error(['Header "',header,'" has no values matching the ones requested'])
         else
            ierr=1;
            index=[];
         end
   end

elseif nargin == 4           % First and last header value specified
   header_vals=s_gh(seismic,string);
      ha=varargin{1};       
      he=varargin{2};
      index=find(header_vals >= ha & header_vals <= he);
      if isempty(index)
         if nargout <= 1
            error(['Header "',header,'" has no values within range specified (',num2str([ha,he]),')'])
         else
            ierr=2;
            index=[];
         end
      end

end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [index,ierr]=find_trace_index(seismic,expression)
% Function finds index of traces whose header values match a logical expression
%
% INPUT
% seismic       seismic data set
% expression    logical expression involving header values
% OUTPUT
% index         index of trace numbers (traces "seismic.traces(:,index)" are selected)
% ierr          error code

global S4M

ierr=0;

words=lower(unique(extract_words(expression)));       % Find all the words in the logical expression

%       Find all the header mnemonics in "words" and assign header values to variables with those names 
if S4M.case_sensitive
   idx=find(ismember(seismic.header_info(:,1),words));
else
   idx=find(ismember(lower(seismic.header_info(:,1)),words));
end

if isempty(idx) && sum(ismember(words,'trace_no')) == 0
  disp([' No header mnemonics found in expression "',expression,'"'])
  disp(' header mnemonics available')
  disp(seismic.header_info(:,1)')
  if nargout == 1
     error(' Abnormal termination')
  else
     ierr=3;
     index=[];
     return
  end
end

nh=length(idx);
for ii=1:nh
  eval([lower(char(seismic.header_info(idx(ii),1))),' = seismic.headers(idx(ii),:);']);
end

                try
index=eval(['find(',lower(expression),')']);

                catch
disp([' The argument of keyword "traces" (',expression,')'])
disp(' is probably not a valid logical expression')
if nargout == 1
   error(' Abnormal termination')
else
   ierr=4;
   index=[];
end
                end
