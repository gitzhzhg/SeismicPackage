function segy = SEGY_ReplaceShotGather(segy, snum, data)

% segy = SEGY_ReplaceShotGather(segy, shotnum, data)
%
% Replaces the shot gather 'shotnum' with 'data'. Does not modify any
% headers or anything else -- it just replaces the trace data itself. You
% must have run SEGY_FindShots() before this will work.
%
% Note that your new shot must be the same size as the old shot. This
% *replaces* a shot in-place.
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

% $Id: SEGY_ReplaceShotGather.m,v 1.1 2008/03/04 22:38:06 cmhogan Exp $


if isnan(segy.sx)
    error('You did not find shots! Use SEGY_FindShots() to find them.');
end

if (snum > length(segy.sx))
    error('Aint that many shots, hotshot.');
end

numtraces = segy.shottraces(snum);

if (size(data, 2) ~= numtraces)
    error('The new shot gather must have the same number of traces as the old one!');
end

skiptraces = 0;

if(snum > 1)
   skiptraces = sum(segy.shottraces(1:(snum-1))); 
end

for idx = 1:segy.shottraces(snum)
   segy = SEGY_ReplaceTrace(segy, skiptraces + idx, data(:, idx)); 
end