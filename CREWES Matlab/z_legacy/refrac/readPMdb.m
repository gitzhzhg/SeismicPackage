% Function: readPMdb
%
% Usage:  [num value] = readPMdb('filename.a_db');
%
% Reads a ProMAX ASCII database output file, returning two vectors
% containing the values of that database order.  For example, if
% you export the receiver X coordinate database, the first vector
% (num) will be the receiver number, and the 2nd vector (value) is
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

% the receiver coordinate.
function [a, b] = readPMdb(filename)
if( nargin < 1 )
   error('readPMdb: No file name specified.');
end
fid = fopen(filename,'r');
if( fid == -1 )
   errs = sprintf('readPMdb: Could not open file %s\n',filename);
   error(errs);
end
% A ProMAX database export file looks like:
%
%
%
%  ASCII database file write for Area="blackfoot", Line="10hzp"
%
%
%  Value list for Database Order SRF
% Surface location X coordinates                                                  
%>SRFGEOMETRYX_COORD 
%        SRF   X_COORD 
%<       101| 348991.22 |
%<       102| 348977.50 |
% Read and skip over the the first 10 lines
for i=1:10
   tmp = fgetl(fid);
end
% Next, read each line, until we don't successfully read 2 numbers
count = 2;
i = 1;
[values count] = fscanf(fid, '< %d| %f |');
pairs = reshape(values,2,count/2)';
a = pairs(:,1);
b = pairs(:,2);
fprintf(1,'readPMdb: read %d data pairs from %s\n',count/2,filename);
fclose(fid);