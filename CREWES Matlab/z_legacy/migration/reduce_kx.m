function [spec,kx]=reduce_kx(inspec,v,inkx,w,dip);
%[spec,kx]=reduce_kx(inspec,vel,inkx,f,dip);
%
%Reduces the number of kx values required to extrapolate a wavefield.
%New nyquist calculated using highest frequency, slowest velocity and highest
%expected dip.
%
%  spec...spectrum of kx reduced input wavefield
%  kx...vector of reduced spatial frequencies
%  inspec...spectrum of input wavefield
%  vel...slowest scalar velocity (one way) - make sure units correspond
%        with inkx and w
%  inkx...vector of input spatial frequency 
%  w...highest temporal frequency
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

%  dip...highest expected dip (deg)
v=real(v);
%***compute new nyquist***
kxn=w*sin(pi*dip/180)/v;%new nyquist
dkx=abs(inkx(2)-inkx(1));
kxn=dkx*(1+fix(kxn/dkx));%ensures existance of kx=0
%*************************
%***compute new kx***
kx=[-kxn:dkx:kxn-dkx];
if abs(kx(1))>abs(inkx(1))
  clear kx;
  kx=inkx;
end
%********************
%***make a reduced spectrum***
kxs=1+(length(inkx)-length(kx))/2;
spec=inspec(:,kxs:kxs+length(kx)-1);
%*****************************
%***taper the edges***
spec(:,1)=.5*spec(:,1);
spec(:,length(kx))=.5*spec(:,length(kx));
%*********************