function [depth] = avgdepth(fbcoord, hg1,standard, depthreject, dev)
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

goodfbc = find(~isnan(fbcoord));
fbca=reshape(fbcoord,1,size(fbcoord,1)*size(fbcoord,2));
%goodfbc = find(~isnan(fbca));
fbcs=sort(fbca(goodfbc));
fbcg=fbcs(1:size(fbcs,2)-1) - fbcs(2:size(fbcs,2));
fbci=[ 1 (find(fbcg~=0)+1)];
coord=fbcs(fbci);
% Define the average depth matricies:
% coord: coordinates from minx to maxx  (spaced by receiver increment)
% depth: accumulating depth values
% fold: depth fold
[tmp ncoords] = size(coord);
depth = NaN.*ones(3, ncoords);
fold = zeros(1, ncoords);
[ndepths endhg] = size(hg1);
alldepth =NaN.*ones(ndepths,ncoords);
avgdepth=NaN.*ones(1,ncoords);
stddepth=NaN.*ones(1,ncoords);
for i=1:ndepths
   fprintf(1,'Averaging for shots %d %d\n',hg1(i,1), hg1(i,2));
   shotnum = hg1(i,1);
   d = hg1(i,3:endhg);
   dgoodi = find(~isnan(d));
   x1 = fbcoord(shotnum, dgoodi(1));         % Coordinate of first good depth
   x2 = fbcoord(shotnum, dgoodi(length(dgoodi)));%assuming that there is no gap
   x1i = find(coord == x1); % Index of this coordinate in the coordinate array
   x2i = find(coord == x2);
   alldepth(i,[x1i:x2i])=d(dgoodi);
%   depth(x1i:x2i) = depth(x1i:x2i) + d(dgoodi);
%   fold(x1i:x2i) = fold(x1i:x2i) + 1;
end
for n=1:ncoords
	validdepth=find(~isnan(alldepth(:,n)));
	[a b]= size(validdepth);
    if( b ~=0 )
	avgdepth(n)=mean(alldepth(validdepth,n));
	stddepth(n)=std(alldepth(validdepth,n));
	fold(n)=a;
    end
end
if (depthreject==1)
  for n=1:ncoords
	validdepth=find(~isnan(alldepth(:,n)));
	[a b]= size(validdepth);
    if( b ~=0 )
	x=alldepth(validdepth,n);
	d=abs(x-avgdepth(n));
	if (standard==1)
	   f=dev * stddepth(n);
	else
	   f=dev;
	end
	
	baddepth=find(d>f);
	[a b]=size(baddepth);
	if (b ~=0)
	   x(baddepth)=NaN*baddepth;
	   alldepth(validdepth,n) = x;
	   validdepth=find(~isnan(alldepth(:,n)));
	   [a b]= size(validdepth);
    	   if( b ~=0 )
		avgdepth(n)=mean(alldepth(validdepth,n));
		stddepth(n)=std(alldepth(validdepth,n));
		fold(n)=a;
	   end
	end
    end
  end
end
depth(1,:)=coord;
depth(2,:)=avgdepth;
depth(3,:)=fold;
depth(4,:)=stddepth;
%goodfolds = find(fold > 0);
%depth(goodfolds) = depth(goodfolds) ./ fold(goodfolds);