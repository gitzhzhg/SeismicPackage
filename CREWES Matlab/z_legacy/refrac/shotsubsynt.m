
% Subtract traveltimes of shot i from j
% i, j = shot numbers
% shotcoord = shot location vector
% fbcoord = receiver location matrix (shot, rec)
% fbtime = fb pick matrix (shot, rec)
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
function [startx, endx, tdiff1, tdiff2,start1index, start2index,i,j] = shotsubsynt(i,j,shotcoord,fbcoord,fbtime)
%traveltime difference between adjacent shot records;
if shotcoord(i)>shotcoord(j)
  k=i;
  l=j;
  j=k;
  i=l;
end
% validxi = 1-isnan(fbcoord(i,:));
% validxj = 1-isnan(fbcoord(j,:));
validxi = find(~isnan(fbcoord(i,:)));
validxj = find(~isnan(fbcoord(j,:)));
[startp1x startp1xi] = min(fbcoord(i,validxi));
[startp2x startp2xi] = min(fbcoord(j,validxj));
startx = max(startp1x, startp2x);
if( startx == startp1x )
   start1index = find(fbcoord(i,:) == startx);
else
   start1index = find(fbcoord(j,:) == startx);
end
jsubpoints1 = find(fbcoord(j,validxj)<shotcoord(i) & fbcoord(j,validxj)>=startx);
isubpoints1 = find(fbcoord(i,validxi)<shotcoord(i) & fbcoord(i,validxi)>=startx);
% Now, work forwards from the beginning of the first section to find
% the index of the end of the first section.
[maxs1x end1index] = max(fbcoord(j,jsubpoints1));
end1index = find(fbcoord(j,:) == maxs1x);
tdiff1 = fbtime(j,jsubpoints1) - fbtime(i,isubpoints1);
[endp1x endp1xi] = max(fbcoord(i,validxi));
[endp2x endp2xi] = max(fbcoord(j,validxj));
endx = min(endp1x, endp2x);
if( endx == endp1x )
   end2index = find(fbcoord(i,:)==endp1xi);
else
   end2index = find(fbcoord(j,:)==endp2xi);
end
jsubpoints2 = find(fbcoord(j,validxj)>shotcoord(j) & fbcoord(j,validxj)<=endx);
isubpoints2 = find(fbcoord(i,validxi)>shotcoord(j) & fbcoord(i,validxi)<=endx);
% Now, work backwards from the end of the second section to find
% the index of the beginning of the second section.
[mins2x start2index] = min(fbcoord(j,jsubpoints2));
start2index = find(fbcoord(j,:) == mins2x);
tdiff2 = fbtime(i,isubpoints) - fbtime(j,jsubpoints);
% Old stock
%xloc=startx:step:endx;
%p1=interp1(fbcoord(i,:),fbtime(i,:),xloc,'linear');
%p2=interp1(fbcoord(j,:),fbtime(j,:),xloc,'linear');
%tdiff2=fbtime(j,startx:endx)-fbtime(i,startx:endx);
%tdiff2=p2-p1;
%start2 = startx;
%end2 = endx;
%step=1;
%xloc=startx:step:endx;
%p1=interp1(fbcoord(i,:),fbtime(i,:),xloc,'linear');
%p2=interp1(fbcoord(j,:),fbtime(j,:),xloc,'linear');
%tdiff1=fbtime(i,startx:endx)-fbtime(j,startx:endx);
%tdiff1=p1-p2;
%start1 = startx;
%end1 = endx;
%endx = shotcoord(j);
%startx = shotcoord(i);