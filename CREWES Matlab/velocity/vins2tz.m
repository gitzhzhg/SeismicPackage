function tzcurve=vins2tz(vins,z,nlegs,tnot)
% VINS2TZ: given instantaeous velocity versus depth compute a two-way time-depth curve
% 
% tzcurve=vins2tz(vins,z,nlegs,tnot)
%
% vins ... column vector in instantaneous velocities
% z ... vector of depths to pair with vins 
% nlegs ... number of points desired on the tz curve
%  ******* default nlegs = length(z) ********  
% tnot ... two-way traveltime to depth z(1)
%  ******* default tnot=0 ********
%
% tzcurve ... matrix of dimension nlegs by 2, first column is two-way
% vertical traveltimes and the second column is the corresponding depths
% The depths are evenly sampled as linspace(min(z),max(z),nlegs)
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

%

if(nargin<4)
    tnot=0.0;
end
if(nargin<3)
    nlegs=length(z);
end

if(nlegs<length(z))
    zout=linspace(min(z), max(z), nlegs);
else
    zout=z;
end

t=2*vint2t(vins,z,zout,tnot/2);%make sure it's two-way time

tzcurve=[t(:) zout(:)];
    