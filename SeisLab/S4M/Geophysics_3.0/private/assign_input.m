function param=assign_input(param,arguments,fcn)
% Input argument "arguments" is a cell array; each cell contains a cell array 
% whose first entry is a string representing a keyword; the other entries 
% represent parameters.
% This function matches these keywords to fields of the structure "param" and 
% replaces the values of these fields with the corresponding paramters
% Used to replace defaults of input arguments with actual arguments.
%
% The actual input arguments can also be provided via the global 
% variable "PARAMETERS4FUNCTION"
%
% Experimental: for SeisLab-type arguments
%
% Written by: E. Rietsch: July 11,2008
% Last updated: January 18, 2009: bug fix
%
%           param=assign_input(param,arguments,fcn)
% INPUT
% param     structure with default values of the input arguments
% arguments cell array, each element of "arguments" is a cell array
%           whose first element is a field name of "param"
% fcn       string with the name of the function calling "assign_input";
%           optional
% OUTPUT
% param    input structure updated with values in "arguments"
%
% UPDATE HISTORY
%

global PARAMETERS4FUNCTION S4M

larguments=length(arguments);
kw=cell(larguments,1);
parameters=cell(length(arguments),1);

for ii=1:larguments
   temp=arguments{ii};
   kw(ii)=temp(1);
   parameters{ii}=temp(2:end);
end

if S4M.case_sensitive
   keywords=fieldnames(param);
else
   keywords=lower(fieldnames(param));
   kw=lower(kw);  % Keywords specified
end

%	Check if arguments are supplied via global variable "PARAMETERS4FUNCTION.fcn.default"

if nargin > 2

   if isfield(PARAMETERS4FUNCTION,fcn)
      temp=PARAMETERS4FUNCTION.(fcn);
      if isfield(temp,'default')  &&  ~isempty(temp.default)
         defaults=temp.default;
         fields=fieldnames(defaults);       
         bool=ismember(fields,keywords);
         if ~all(bool)
            disp(['Parameters specified via "PARAMETERS4FUNCTION.',fcn,'.default":'])
            disps(cell2str(fields,', '))

            fields=fields(~bool);
            disp('Parameters that are not keywords of function:')
            disps(cell2str(fields,', '))
  
            disp('Possible keywords are: ')
            disps(cell2str(keywords,', '))
            temp.default=[];
%	Set "PARAMETERS4FUNCTION.functionname.default" to the empty matrix to prevent
                                % it from being used again in another function
	    PARAMETERS4FUNCTION.(fcn)=temp; 
	
            error(['Not all fields of "PARAMETERS4FUNCTION.',fcn,'.default" are keywords'])
         end
  
         for ii=1:length(fields)
            param.(fields{ii})=temp.default.(fields{ii});
         end

%     Set "PARAMETERS4FUNCTION.functionname.default" to the empty matrix to prevent
                                % it from being used again in another function
         temp.default=[];
         PARAMETERS4FUNCTION.(fcn)=temp; 
      end
   end
end

%     Use input arguments of the function calling "atualInputArguments"
largs=length(arguments);
% if rem(largs,2) > 0
%   error('Number of keyword-controlled input arguments must be even.')
% end


%     Check if the keywords specified, "kw", are the ones actually 
%     expected, "keywords"; expand them if required
ier=false;
for ii=length(kw):-1:1;
   kw{ii}=kw_no1(keywords,kw{ii});
   if isempty(kw{ii})
      ier=true;
   else
      temp=parameters{ii};
      if length(temp) == 1
         param.(kw{ii})=temp{1};
      else
         param.(kw{ii})=temp;
      end
   end
end

if ier 
   temp=dbstack;
   [dummy,funct]=fileparts(temp(2).name);
   error(['There is a problem with the input arguments of function "',funct,'".'])
end

if nargin > 2
   temp1.actual=param;
   PARAMETERS4FUNCTION.(fcn)=temp1; 
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function kw=kw_no1(keywords,kw)

global S4M

idx=find(ismember(keywords,kw));
if length(idx) == 1
   return
end


%	Handle case of keyword expansion (if allowed)
if isempty(idx) && S4M.keyword_expansion
   starts=strmatch(kw,keywords);    % Find all the keywords which begin with "kw".
   if length(starts) == 1
      kw=keywords{starts};
   elseif length(starts) > 1
      disp([' Keyword "',kw,'" is not a unique abbreviation. Possible expansions are:'])
      disps(cell2str(keywords(starts),', '))
      kw='';
   else
      disp([' Keyword "',kw,'" is not one of those expected and cannot be expanded to match one.'])
      disp(' Valid keywords are:') 
      disps(cell2str(keywords,', '))
      kw='';
   end
else
   disp([' Keyword "',kw,'" is not one of those expected. Valid keywords are:']) 
   disps(cell2str(keywords,', '))
   kw='';
end
