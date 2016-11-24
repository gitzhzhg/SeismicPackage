function thead = SEGY_ReadTextHeader(segy, format)

% thead = SEGY_ReadTextHeader(segy, format)
% 'segy' is a SEGY structure, open for reading by SEG_OpenFile().
% 'format' is either 'ascii' or 'ebcdic', and specifies the format in which
% the SEGY header was written.
% thead will be returned in ascii format.
%
% Chad Hogan, 2008
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

% $Id: SEGY_ReadTextHeader.m,v 1.2 2009/07/24 15:49:19 kwhall Exp $


if nargin < 2
    format = 'ascii';
end

% Make sure we're at the start of the file
% Assume only one textual header for now
if(fseek(segy.FILE, 0, 'bof') ~= 0)
    % rewind the file to the textual header start
    disp(ferror(FILE));
    error('seeking failed');
end

% First we'll grab a template header to fill in
thead = SEGY_GetTextHeader;

thead = strcat(fread(segy.FILE, 3200, 'char'));

if strcmp(lower(format), 'ebcdic')
    thead = ebcdic2ascii(thead);
end