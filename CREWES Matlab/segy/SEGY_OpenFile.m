function segy = SEGY_OpenFile(fname, mode, endian, textfmt, iwarn)

% segy = SEGY_OpenFile(fname, mode, endianness, textfmt)
% 
% Returns a structure that has a filehandle, along with some useful
% information like:
%
% segy.FILE ........ the filehandle
% segy.numtraces ... the number of traces in the file
% segy.thead ....... the text header
% segy.bhead ....... the binary header
%
% This structure is ready for immediate trace extraction, using the
% SEGY_ReadTrace() function.
%
% See fopen() for details on 'mode' and 'endianness' choices.
% txtfmt = 'ascii' or 'ebcdic'
%
% The returned structure also contains some empty arrays like segy.idxs,
% and some NaN-valued elements like segy.sx, segy.sy. These are used in
% in other functions.
%
% ALL FILES OPENED WITH SEGY_OPENFILE SHOULD BE RELEASED WITH
% SEGY_RELEASEFILE
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

% $Id: SEGY_OpenFile.m,v 1.3 2009/07/24 15:49:19 kwhall Exp $

if(nargin<5)
    iwarn=1;
end
if nargin < 4
    textfmt = 'ascii';
end

segy.sx = NaN;
segy.sy = NaN;
segy.cmps = NaN;
segy.shotgathersize = NaN;
segy.idxs = [];

segy.FILE = fopen(fname, mode, endian);

if(segy.FILE == -1)
    error('OPEN FAILED'); 
end

segy.thead = SEGY_ReadTextHeader(segy, textfmt);
segy.bhead = SEGY_ReadBinaryHeader(segy);

fseek(segy.FILE, 0, 'eof');
allbytes = ftell(segy.FILE);

tracebytes = allbytes - 3600;
segy.numtraces = tracebytes / segy.bhead.tracebytelen;

if(mod(segy.numtraces, 1) ~= 0 && iwarn==1)
    warning('Number of traces is not an integer! Verify that SEGY data contains proper headers etc.');
end

if(iwarn==1)
    disp(['SEGY opened. ' num2str(segy.numtraces) ' traces found, sampling at ' ...
        num2str(segy.bhead.hdt/1000) ' ms']);
end