% Function fflat - Finds the portion of a curve, starting at one
% end, which is flat to within a slope of 'tolerance'
%
% Usage:   [p1 p2] = fflat(x,y,tolerance,beginning);
%
%      x, y - data defining the curve
% tolerance - the maximum slope of the flat portion
% beginning - 1==start at minimum coordinate  -1==start at maximum coord
% 
% p1, p2 - indexes of the flat portion of the curve
% If a straight segment cannot be found, then p1 and p2 
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

% are both returned empty ([])
function [p1, p2] = fflat(x, y, tolerance, beginning)
curvestart = 1;
curveend = length(y);
if( beginning == 1 )
   p1 = curvestart;
   p2 = curveend;
   dir = 1;
else
   p1 = curveend;
   p2 = curvestart;
   dir = -1;
end
% If the minimum x-coordinate is at the start of the x array (x(1))
% then the coordinates increase with the index numbers
xminindex = find(x == min(x));
if( xminindex == 1)
   reverse = 0;
else
   reverse = 1;
end
% If the coordinate direction is reversed, swap p1 and p2,
% and set the search direction to be the other way
% if( reverse )
%    tmp = p1;
%   p1 = p2;
%   p2 = tmp;
%   dir = -dir;
% end
p2max = curveend-1;
p2min = curvestart+1;
dist = dir*length(y);
done = 0;
while ~done
   lastp2 = p2;
   [p tmp] = polyfit(x(p1:dir:p2), y(p1:dir:p2), 1);
   slope = p(1);
   dist = dir*floor(abs(dist)/2);
%   fprintf(1, 'slope: %f  p2: %d dist: %d\n', slope, p2, dist);
   if( abs(slope) < tolerance )
      p2 = floor(p2 + dist);
   else
      p2 = floor(p2 - dist);
   end 
   p2 = min(p2, p2max);
   p2 = max(p2, 2);
   done = (abs(dist)==1 | p2==p2max | p2==p2min );
end
% Now we have a close answer.  
% Do a linear search until we hit the tolerance limit.
if abs(slope) < tolerance 
   sdir = dir;
%   fprintf('Doing linear search to increase length\n');
else
   sdir = -dir;
%   fprintf('Doing linear search to decrease error\n');
end
done = 0;
while ~done
   lastp2 = p2;
   lastslope = slope;
   p2 = min(p2+sdir, p2max);
   p2 = max(p2,p2min);
   [p tmp] = polyfit(x(p1:dir:p2), y(p1:dir:p2), 1);
   slope = p(1);
%   fprintf(1, 'slope: %f  p2: %d dist: %d\n', slope, p2, dist);
   if(sdir == dir)
      done = (abs(slope)>tolerance);
   elseif(sdir == -dir)
      done = (abs(slope)<tolerance);
   end
   done = done | (p2==p2max) | (p2==p2min);
end
% The linear search will go one point too far.  Backup if needed.
if( abs(slope) > tolerance )
   p2 = lastp2;
   slope = lastslope;
end
fprintf(1, 'final slope: %f  p2: %d dist: %d\n', slope, p2, dist);
% Define the 'flat spot not found' condition.
if( (abs(slope) > tolerance) | (abs(p1-p2) < 5) )
%   figure; 
%   plot(x,y, 'g');
   p1 = [];
   p2 = [];
else
%   line([x(p1) x(p2)], [y(p1) y(p2)]);
end