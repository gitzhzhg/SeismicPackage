function val = toInterpretedString ( obj )
%
%function val = toInterpretedString ( obj )
%
%   Reads header interpretations from a comma separated values (.csv)
%   spreadsheet
%
%   Assumptions:
%       - Spreadsheet contains 2 columns
%       - First row of spreadsheet contains column names
%       - Column names are:
%         code, description
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

%

val='you fail';

%Which spreadsheet file has the definitions
definitionsFileName = [obj.headerType '.' obj.headerWord '.csv'];

%Does that spreadsheet file exist
if(exist(definitionsFileName,'file'))

    definitionsFile = File(definitionsFileName);

    %Get first row with column names, just in case (not needed?)
    k = textscan(definitionsFile.fid,'%q%q',1,'Delimiter',',');

    %Read the rest of the definitions
    v = textscan(definitionsFile.fid,'%q%q','Delimiter',',');
    M = containers.Map(v{1},v{2});

    try
        val = M(obj.value);
    catch ex
        %disp (ex.message)
        val = toString(obj);
    end    
else
    %return number
    val = toString(obj);    
end

return


%Get first row with column names, just in case (not needed if cols arre 
%in the right order)
%k = textscan(definitionsFile.fid,'%q%q',1,'Delimiter',',');

%Read the rest of the definitions
%v = textscan(definitionsFile.fid,'%q%q','Delimiter',',');


% [m n] = size(v)
% if m < 2
%   if strcmp(char(v(1)),code)
%       val = v(2);
%   else
%       val = code;
%   end
%   
%   return
% end

% rows = cat(2,v{:});
% 
% M = containers.Map(...
%     rows(1,:),...
%     rows(2,:));
% 
% code
% 
% val = codeRows(code);



end