% Subtract traveltimes of shot i from j
% i, j = shot numbers
% r = shot location vector
% reclocation = receiver location matrix (shot, rec)
% shotpick = fb pick matrix (shot, rec)
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

% step = interpolation interval 
function [start1, end1, tdiff1, start2, end2, tdiff2] = shotsubup(i,j,r,reclocation,pickuphole,step)
%traveltime difference between adjacent shot records;
p1 = NaN;
p2 = NaN;
endx = r(j);
startp1x=min(reclocation(i,:));
startp2x=min(reclocation(j,:));
startx = max(startp1x, startp2x);
step=1;
xloc=startx:step:endx;
p1=interp1(reclocation(i,:),pickuphole(i,:),xloc,'linear');
p2=interp1(reclocation(j,:),pickuphole(j,:),xloc,'linear');
tdiff1=p1-p2;
start1 = startx;
end1 = endx;
endp1x=max(reclocation(i,:));
endp2x=max(reclocation(j,:));
endx = min(endp1x, endp2x);
startx = r(i);
xloc=startx:step:endx;
p1=interp1(reclocation(i,:),pickuphole(i,:),xloc,'linear');
p2=interp1(reclocation(j,:),pickuphole(j,:),xloc,'linear');
tdiff2=p2-p1;
start2 = startx;
end2 = endx;