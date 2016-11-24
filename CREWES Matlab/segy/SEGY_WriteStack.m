function SEGY_WriteStack(fname, stack, dt, separation, texttype, numtype)
% SEGY_WRITESTACK(fname, stack, dt, separation, texttype, numtype)
%
% Writes to the 'fname' file in SEGY, a 'stack' (NxM array, N samples 
% in M traces) with a sample interval of 'dt' seconds, a trace-to-trace 
% horizontal 'separation' distance, a 'texttype' equal to 'ebcdic' 
% or 'ascii', and a 'numtype' of 'l' for little-endian or 'b' for 
% big-endian.
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

% $Id: SEGY_WriteStack.m,v 1.2 2004/07/30 21:23:35 kwhall Exp $
bhead  = SEGY_GetBinaryHeader;

if (strcmpi(texttype, 'ascii'))
    thead  = SEGY_GetTextHeader;
else
    thead = ascii2ebcdic(SEGY_GetTextHeader);
end

bhead.hdt = dt * 1000000; % dt is in s, but hdt in microseconds.
bhead.dto = bhead.hdt;

[tracesamps, numtraces] = size(stack);

% Now we set the number of samples.
bhead.hns = tracesamps;
bhead.nso = bhead.hns;

% Choose our endianness.
if (strcmpi(numtype, 'l'))
    machine = 'ieee-le';
    warning('Writing in non-standard little endian format');
else
    machine = 'ieee-be';
end

% open the file
[FILE, message] = fopen(fname, 'w', machine);
if (message); warning(message); end

SEGY_WriteTextHeader(FILE, thead);
segytrace = SEGY_GetTrace;
segytrace.id = 1;         % just one id in a file.
segytrace.dt = bhead.hdt; % time delta t
segytrace.ns = bhead.hns; % how many samples?

bhead.tsort = 4; % stacked
SEGY_WriteBinaryHeader(FILE, bhead);    
for i = 1:numtraces
    segytrace.cdp    = i; 
    segytrace.cdpt   = i; % trace number within gather
    segytrace.sx     = separation * (i - 1); % source location
    segytrace.gx     = segytrace.sx;         % group location
    segytrace.offset = 0;
    segytrace.data   = stack(:, i);
    SEGY_WriteTrace(FILE, segytrace, segytrace.ns);
end

fclose(FILE);