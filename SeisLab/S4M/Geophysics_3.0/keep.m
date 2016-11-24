function keep(varargin)
% Remove all variables from the workspace except for those in the argument
% list;
%
% Written by: E. Rietsch: January 7, 2008
% Last updated:
%
%            keep(varargin)
% INPUT
% varargin   one or more strings with the names of variables in the workspace,
%            'strings that do not correspond to variable names are ignored.
%            Use of the wildcard character '*' is permitted.
%            Since all the input arguments are strings they can be written as a 
%            space-separated list (see example below).
%
% EXAMPLE:
%            a1=1; b1=2; b2=20; c3=3; temp=0; temp1=1;
%            whos
%            keep b* temp
%            whos


%       If the argument list is empty or if it includes a single asterisk then
%       keep all variables
if isempty(varargin) || any(strcmp('*',varargin))
   return
end

%       Find all variables in the caller workspace
ws=evalin('caller','who');

%       Return if the workspace is empty (nothing to keep)
if isempty(ws)
   return
end

%      Prepend and append blanks to the file names; this means that there will
%      be a match only if the whole file name is the same (except for wildcards)
ws=cellfun(@(x) [' ',x,' '],ws,'UniformOutput',false); 

%       Create a logical matrix the values of which are true if the 
%       workspace variable is not matched by a particular input string
lvarargin=length(varargin);
bool=false(length(ws),lvarargin);
for ii=1:lvarargin
   name=strrep(varargin{ii},'*','\w*');
   bool(:,ii)=cellfun(@isempty,regexp(ws,['\s',name,'\s']));
end
bool=all(bool,2);

%       Remove variables that should not be kept
if any(bool)
   evalin('caller',['clear ' cell2str(ws(bool))]);
end
