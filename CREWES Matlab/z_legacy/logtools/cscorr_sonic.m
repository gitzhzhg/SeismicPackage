function [sout,tnot,delt,zcs2,tcs2,deltcs]=cscorr_sonic(slog,zlog,tcs,zcs)
% [sout,tnot]=cscorr_sonic(slog,zlog,tcs,zcs)
%
% CSCORR_SONIC performs check shot corrections on a sonic log.
% The algorithm (devised by Clint Frasier) is:
%	-Integrate the sonic to get traveltime
%	-Take the difference between the sonic derived traveltime and
%		the checkshot traveltime at the depths of the check shot
%		recordings.
%	-Using a spline interpolator, estimate a dt at every sampled 
%		depth of the sonic log.
%	-The corrected sonic is the depth derivative of the sum of the
%		integrated sonic and the spline interpolated differences
%
%	slog = vector of sonic log samples
%	zlog = vector of depths for slog. Must be same size vector as slog
%	tcs  = vector of check shot times (1-way)
%	zcs  = vector of check shot depths
%
%	sout = check shot corrected sonic
%	tnot = one-way time constant that must be added to integrated sonic
%			one-way times to match check shot times.
%
% G.F, Margrave
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
%other returned variables
%	[tcs2,zcs2] ... modified check shot array actually used
%	deltcs ... delta t's at the depths zcs2
%	delt ... spline fit to deltcs
%make everything a column vector
slog=slog(:);
zlog=zlog(:);
tcs=tcs(:);
zcs=zcs(:);
%integrate the sonic
nz=length(zlog);
%generate a vector of differences
dz=diff(zlog);
%a vector of the average of adjacent points on the sonic
%s2=(slog(1:nz-1)+slog(2:nz))/2;
%tson=zeros(size(zlog));
%integrate by summing the array product of dz and s2
%tson(2:nz)=2*(1.e-06)*cumsum(dz.*s2);
tson=.5*int_sonic(slog,zlog);%make sure its 1-way time
tson=tson(:);
%find those points on the checkshot between the start and end
%of the log
ind=between(zlog(1),zlog(nz),zcs,2);
tcs2=tcs(ind);
zcs2=zcs(ind);
%see if we need to interpolate a point at the beginning
if(ind(1)~=1)
	znot=zlog(1);
	tnot=interp1(zcs,tcs,znot);
	tcs2=[tnot;tcs2];
	zcs2=[znot;zcs2];
else
	tnot=tcs2(1);
end
%see if we need a point at the end
if(ind(length(ind))~=length(zcs))
	zend=zlog(nz);
	tend=interp1(zcs,tcs,zend);
	tcs2=[tcs2;tend];
	zcs2=[zcs2;zend];
end
%find times on integrated sonic at each checkshot depth
tson_cs= interp1(zlog,tson,zcs2);
%fit a spline to the time differences. Use only those log depths within
%the checkshot range
ind=between(zcs2(1),zcs2(length(zcs2)),zlog,2);
deltcs=tcs2-tson_cs;
delt=spline(zcs2,deltcs,zlog(ind));
delt=delt(:);
% pad beginnig and end of delt with constant time differences
if(ind(1)~=1)
	npad=ind(1)-1;
	delt=[delt(1)*ones(npad,1);delt];
end
if(ind(length(ind))~=nz)
	npad=nz-ind(length(ind));
	delt=[delt;delt(length(delt))*ones(npad,1)];
end
%add to the integrated sonic
%tson=tson+delt(:);
%recompute the sonic log
%sout= gradient(tson',zlog')*(1.e06);
%add the gradient of delt to the sonic
ds=gradient(delt',zlog)*(1.e06);
sout=slog+ds.';
%[m,n]=size(slog);
%if(n==1)
%	sout=sout.';
%end