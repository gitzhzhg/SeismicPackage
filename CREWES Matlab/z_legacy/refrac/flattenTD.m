% flattenTD - flattens the TD curve (td) to the reference TD curve
% (tdref) and returns the flattened curve and velocity corresponding
% to the timeshift required to flatten them.
%
% Input:
%   td     - TD curve to flatten
%   tdref  - reference curve.  TD is flattened to tdref
%   sp, ep - start and end index of the 'flat' portion of TD
%   spref, epref - start and end index of the flat portion of TDref
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

%   x      - seperation between the shots for TD and TDref
function [newtd, vel] = flattenTD(td, tdref, sp, ep, spref, epref, x)
if( sp<ep )
   dir = 1;
   p1 = max(sp, spref);
   p2 = min(ep, epref);
   ok = (p1<p2);
else
   dir = -1;
   p1 = min(sp, spref);
   p2 = max(ep, epref);
   ok = (p1>p2);
end
% The 'ok' flag is true if the two curves overlap
if( ok )
   fprintf(1, 'flatten: p1:%d  p2:%d\n', p1, p2);
   timeshift = mean(td(p1:dir:p2) - tdref(p1:dir:p2));
   vel = x / timeshift;
   newtd = td - timeshift;
   fprintf(1,'flatten: timeshift %f  vel: %f\n', timeshift, vel);
else
   fprintf(1,'flattenTD: curves did not overlap.\n');
end