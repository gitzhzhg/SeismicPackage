function SEGY_WriteTextHeader(FILE, thead, format)
% SEGY_WRITETEXTHEADER(FILE, thead, format)
%
% This function writes a text header 'thead' to the file FILE with the
% 'format', where 'format' is either 'ebcdic' or 'ascii'. 'format' is an
% optional argument, and if unspecified will default to 'ascii'. You can
% retrieve a valid text header from the function GETTEXTHEADER. It is
% wise to get a text header from this function and then edit it as you
% see fit.
%
% This function will OVERWRITE whatever text header may be existing in
% the file. 
%
% Chad Hogan, 2004
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

% $Id: SEGY_WriteTextHeader.m,v 1.1 2004/06/18 21:24:29 cmhogan Exp $

if nargin < 3
    format = 'ascii';
end

if strcmp(lower(format), 'ebcdic')
    thead = ascii2ebcdic(thead);
end
    

if length(thead) ~= 3200
    error('input text header must be exactly 3200 bytes long');
    return
end

fseek(FILE, 0, 'bof'); % rewind the file to the very start

if(fwrite(FILE, thead) ~= 3200)
    disp(ferror(FILE))
    error('writing failed');
end