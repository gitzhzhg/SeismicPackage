function SEGY_WriteGathers(fname, gather, dt, gathtype, geom1, geom2, texttype, numtype)
% SEGY_WRITEGATHERS(filename, gather, dt, gathtype, geom1, geom2, texttype, numtype)
%
% Writes SEGY gathers of varying geometry. You can write shot gathers or
% CDP gathers with this particular version. This is the automated version
% of SEGY writing, and so it is necessarily somewhat simplified. Read
% through this information and decide if it will suit your purposes. If it
% cannot accomplish your task, you can easily use SEGY_GETBINHEADER, 
% SEGY_WRITEBINHEADER,  SEGY_GETTEXTHEADER, SEGY_WRITETEXTHEADER, 
% SEGY_GETTRACE and SEGY_WRITETRACE to write the file yourself. It's not
% that bad. Really. 
%
% For shot gather geometry calculations, it is assumed that the shot
% gathers have a symmetrical distribution around the source, and that there
% is a single zero-offset trace. I imagine that this will only work for
% synthetic data. Again, if you don't like this, you'll have to do it
% manually. 
%
% ---------------------------------
% filename: name of the file to save this gather into. If this file exists,
% it will be wiped out without mercy or second chances.
% ---------------------------------
% gathers: the set of gathers for the seismic data. This should be a cell
% array, with each element of the cell array as an array of traces.
% Therefore, the entire third gather will be referenced as gathers{3}. If we
% take this gather like this:
%
% thirdgather = gathers{3};
%
% Then we can read the fourth trace in that gather with something like
% this:
%
% fourthtrace = thirdgather(:, 4);
%
% It is assumed that the traces are sorted in order of increasing offset
% within the gathers. The first trace in a gather is zero offset, and the
% last trace is the longest offset. 
%
% ---------------------------------
% dt: the sampling interval, in seconds. 
% ---------------------------------
% gathtype: 'cdp' or 'shot', for CDP gathers or shot gathers respectively.
% You'll lose some information with each approach. For example, you can't take
% a CDP gather written by this function and resort it into a shot gather,
% because the shot number information is not written to the file. If this
% is a problem, you will have to write the file manually using
% SEGY_GETBINHEADER, SEGY_WRITEBINHEADER, SEGY_GETTEXTHEADER,
% SEGY_WRITETEXTHEADER, SEGY_GETTRACE, and SEGY_WRITETRACE
% ---------------------------------
% geom1: This is the index that separates each gather.
%        CDP: distance separating each depth point (mid point, whatever). 
%       Shot: distance between each shot point.
% ---------------------------------
% geom2: This is the index that separates each trace within a gather.
%        CDP: offset interval from the shot to the receiver
%       Shot: offset interval between receivers
% ---------------------------------
% texttype: 'ebcdic' or 'ascii'
% ---------------------------------
% numtype: 'b' for big-endian, or 'l' for little-endian. 
% ---------------------------------
%
% Chad Hogan, 2004
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

% $Id: SEGY_WriteGathers.m,v 1.3 2008/03/05 00:47:41 cmhogan Exp $

bhead  = SEGY_GetBinaryHeader;

if (strcmpi(texttype, 'ascii'))
    thead  = SEGY_GetTextHeader;
else
    thead = ascii2ebcdic(SEGY_GetTextHeader);
end

bhead.hdt = dt * 1000000; % dt in s, hdt in micros.
bhead.dto = bhead.hdt;
numgathers = size(gather, 2);
bhead.fold = numgathers;


% We have to find the length of the biggest traces in the gather.
biggest = 0; % everything is smaller than Inf, right?
for i = 1:numgathers
    thissize = size(gather{i}, 1);
    if (thissize > biggest); biggest = thissize; end
end


% Now we set the number of samples.
bhead.hns = biggest;
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

% Ok, now we split off into CDP or SHOT gathers. This geometry stuff is a
% bit dodgy so I really think it has to be tested or something. I'm kinda
% making this all up as I go.
if (strcmpi(gathtype, 'cdp'))
    bhead.tsort = 2; % the code for CDP gathers
    SEGY_WriteBinaryHeader(FILE, bhead);    
    for i = 1:numgathers
        numtraces  = size(gather{i}, 2);
        segytrace.cdp  = i; % set the cdp number
        thisgather = gather{i};
        cdploc = (i - 1) * geom1;
        for j = 1:numtraces
            segytrace.cdpt   = j; % trace number within gather
            segytrace.tracl  = ((i - 1) * numtraces) + j; % trace number in the line
            segytrace.tracr  = segytrace.tracl;
            segytrace.fldr   = segytrace.tracl; % field record num
            segytrace.sx     = cdploc - (j - 1) * (geom2 / 2); % source location
            segytrace.gx     = cdploc + (j - 1) * (geom2 / 2); % group location
            segytrace.offset = (j - 1) * geom2;
            segytrace.data   = thisgather(:, j);
            SEGY_WriteTrace(FILE, segytrace, segytrace.ns);
        end
    end
elseif (strcmpi(gathtype, 'shot'))
    bhead.tsort = 1; % sorted as shot I guess. 
    SEGY_WriteBinaryHeader(FILE, bhead);
    for i = 1:numgathers
        numtraces = size(gather{i}, 2);
        segytrace.cdp = i; % Remember that "cdp" is just ensemble number.
        thisgather = gather{i};
        segytrace.sx = geom1; % One shot for a shot gather, obviously. 
        firstgx = segytrace.sx - (floor(numtraces / 2) * geom2);
        for j = 1:numtraces
            segytrace.cdpt   = j; % trace number within the gather
            segytrace.tracl  = ((i - 1) * numtraces) + j; % trace number within the line
            segytrace.tracr  = segytrace.tracl;
            segytrace.fldr   = segytrace.tracl;
            segytrace.gx     = firstgx + (numtraces - 1) * geom2;
            segytrace.offset = abs(segytrace.sx - segytrace.gx); % I hope offsets are always positive.
            segytrace.data = thisgather(:, j);
            SEGY_WriteTrace(FILE, segytrace, segytrace.ns);
        end
    end
end

fclose(FILE);