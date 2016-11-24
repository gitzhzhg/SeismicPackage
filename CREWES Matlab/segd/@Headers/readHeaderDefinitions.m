function obj = readHeaderDefinitions ( obj )
%
%function obj = readDefinitions (obj)
%
%   Reads header definitions from a comma separated values (.csv)
%   spreadsheet
%
%   Assumptions:
%       - Spreadsheet contains 7 columns
%       - First row of spreadsheet contains column names
%       - First column of spreadsheet contains header word names
%       - column names are:
%         name,startByte,endByte,startNibble,endNibble,format,description
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

%Which spreadsheet file has the definitions
definitionsFile = File([obj.headerType '.csv']);

%Get the column names (first row of spreadsheet)
k = textscan(definitionsFile.fid,'%q%q%q%q%q%q%q',1,'Delimiter',',');
obj.headerDefCols = containers.Map(cat(2,k{:}),1:length(k));

%Read the rest of the spreadsheet
v = textscan(definitionsFile.fid,'%q%q%q%q%q%q%q','Delimiter',',');
obj.headerDefs = cat(2,v{:});

%Get the row names (first column of spreadsheet)

[m n] = size(obj.headerDefs);

obj.headerDefRows = containers.Map(...
    obj.headerDefs(:,obj.headerDefCols('name')),...
    1:m);

definitionsFile.closeFile();

end