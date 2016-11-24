function obj = splitLas(obj)
%
% function obj = splitLas(obj)
%
%Break up character lines from LAS  into an cell array array of structures
%
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE


%Linetype1: ~SECTION NAME
%Linetype2: ddd.ddd [comma, space or tab] ccccc cccc cc [comma, space or tab] ...
%Linetype3: ccc cc ccc [comma, space or tab] ddd.ddd .... *** NOT HANDLED ***
%Linetype4: MNEMONIC .UNITS  VALUE1 VALUE2  : COMMENT {FORMAT} | DEFINITION
% Pattern definitions used for regexp
%   ?<variablename> = variablename to assign a match
%   ^               = match beginning of string
%   [ ]             = match any characters listed in the square brackets
%   [^]             = match any characters not listed in the square brackets
%   \s              = white space characters
%   \d              = digit
%   \S              = non-white space characters
%   ()              = group
%   ?               = match zero or one character
%   *               = match zero or more characters
%   +               = match one or more characters
%   .               = wildcard, stands in for any character
%   |               = logical or
%   \|              = '|'
%   ~               = '~'

try
    pattern = [                  ...
        '^\s*(?<section>^~.*)|'   ... %Section name if first char is '~', or
        '^\s*(?<comment>^#.*)|'   ... %Comment if first char is '#' (place in mnemonic), or
        '^\s*(?<mnemonic>[^.]*)\.(?=[^\d])' ... %Mnemonic if the first '.' is not followed by a digit
        '(?<units>\S*)'          ... %Units
        '\s*(?<value>[^:]*)'     ... %Value
        '\:?\s*'                 ... %Single ':' followed by zero or more spaces
        '(?<description>[^|{]*)' ... %Description, all characters left of a '{' or '|'
        '{?(?<format>[^|}]*)}?'  ... %Data format; eg. {F}, {E}, {S},...
        '\s*\|?\s*'              ... %Zero or more spaces, single '|', zero or more spaces
        '(?<association>.*)|'      ... %Everything else => should be a section name, or
        '^\s*(?<data>.*)'        
        ];
    
     s = regexp(obj.sections, pattern, 'names');

    % simplify structure
     s = cell2mat(s);
    
    % convert to cell array and remove trailing whitespace from
    % every element in the cell array
     obj.sections = {'all',strtrim(struct2cell(s))};
    
catch ex
     warning('crewes:las:splitlas',ex.message);
end

end