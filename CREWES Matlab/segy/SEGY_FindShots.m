function segy = SEGY_FindShots(segy)

% segy = SEGY_FindShots(segy)
%
% Finds unique shot locations segy.sx and segy.sy. Also sets the number of
% traces for that shot in segy.shottraces.
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

% $Id: SEGY_FindShots.m,v 1.1 2008/03/04 22:36:42 cmhogan Exp $

disp(['Going to search ' num2str(segy.numtraces) ' traces']);

sx = [];
sy = [];
shottraces = [];

nt = 0;

for idx = 1:segy.numtraces
    SEGY_TraceSeek(segy, idx);
    fseek(segy.FILE, 72, 0);    % move to sx
    thissx = fread(segy.FILE, 1, 'int');
    thissy = fread(segy.FILE, 1, 'int');
    nt = nt + 1;

    if (idx == 1)
        lastx = thissx;
        lasty = thissy;
    end
    
    % If this is a unique shot that we haven't found before
    if(lastx ~= thissx || lasty ~= thissy)
        lastx = thissx;
        lasty = thissy;
        sx(end+1) = thissx;
        sy(end+1) = thissy;
        shottraces(end+1) = nt;
        nt = 0;
    end
    if(mod(idx, 1000) == 0)
        disp(['done ' num2str(idx) ' of ' num2str(segy.numtraces)]);
    end
end
disp(['Found ' num2str(length(sx)) ' unique shot locations']);

segy.sx = sx;
segy.sy = sy;
segy.shottraces = shottraces;