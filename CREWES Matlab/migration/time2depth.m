function [ztrc,z]=time2depth(trc,t,tzcurve,dz)
% TIME2DEPTH: Convert a trace from time to depth by a simple stretch
%
% [ztrc,z]=time2depth(trc,t,tzcurve,dz)
%
% TIME2DEPTH converts a single trace from time to depth. The conversion is
% specified by a time-depth curve that is stored in a two-column matrix.
% The first column is a list of times and the second is a list of 
% corresponding depths. The times can be either one-way or two-way and
% need only be consistent with the time-coordinate vector of the input.
% Between points in the time-depth curve, depths are interpolated linearly.
% Note that this function does not apply an antialias filter. It is
% up to the user to ensure that dz is sufficiently small to preclude
% aliasing.
% 
% trc ... the trace in time
% t ... time coordinate vector for trc
% tzcurve ... an n-by-2 matrix giving the time-depth curve.
%	n is the number of points on the curve and is arbitrary.
%	(more points is usually more accurate.) The first column is
%	time and the second column is the depths that correspond to
%	the times in the first column. The first time should be zero
%	and the last should be greater than or equal to max(t). This can be
%	created with sonic2tz.
% dz ... depth sample size. 
%
% NOTE: to avoid aliasing pick dz<vmin*dt/n where n is 1 for one-way time
% and 2 for 2way time and vmin is the slowest velocity in the model.
%
% G.F. Margrave, CREWES, Nov, 2000
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

%input trace must be regularly sampled
if(sum(abs(diff(diff(t))))>.00001)
    error('input time trace must be sampled regularly');
end


%check tz curve limits
tz=tzcurve(:,1);zt=tzcurve(:,2);
if(min(tz)~=0)
	error('tz curve must start at zero time');
end
if(max(tz)<max(t))
	error('tz curve must extend to times greater than max(t)');
end

%make sure depths are monotonic
ztest=diff(zt);
ind=find(ztest<=0, 1);
if(~isempty(ind))
	error('depths on tzcurve must increase monotonically')
end

%determine depth range
z1=pwlint(tz,zt,t(1));
z2=pwlint(tz,zt,max(t));

z=((0:round((z2-z1)/dz))*dz)';

%ztrc=zeros(size(z));

%interpolation sites
tint=pwlint(zt,tz,z);

%sinc function interpolation
ztrc=sinci(trc,t,tint);