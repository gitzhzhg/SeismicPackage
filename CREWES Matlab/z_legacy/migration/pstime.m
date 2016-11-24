function out=pstime(inspec,vel,inkx,f,dt,dip)
%
%out=ps(inspec,vel,inkx,f,dt,dip);
%
%Advance wavefield one depth step by constant velocity phase shift.
%
%  out...spectrum of extrapolated wavefield
%  inspec...spectrum of input wavefield
%  vel...scalar velocity - make sure units correspond
%        between x, dz, inkx, f
%  inkx...spatial frequencies
%  f...temporal frequencies
%  dz...depth through which to extrapolate
%  dip...dip cut off (degrees, used to limit kx - not a dip filter)
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

%***kx limit the input data***
[rf cf]=size(f);
[spec,kx]=reduce_kx(inspec,vel,inkx,f(rf),dip);
%*****************************
%***get sizes of things***
[rins cins]=size(inspec);clear inspec;
[rs cs]=size(spec);
%*************************
%***initialize some variables***
vel2=vel^2;
kxkx=kx.*kx;
phiout=zeros(rs,cs);
eta=zeros(1,cs);
gazx=zeros(1,cs);
%*******************************
%***extrapolate each frequency***
for j=2:rf
  eta= f(j)*(sqrt(1-vel2*kxkx/(f(j)*f(j)))-1);
  %kz=sqrt((f(j)*f(j)/vel2)-kxkx);
  %kz=real(kz)+i*abs(imag(kz));%Evanecent inverter
  eta=real(eta)+i*abs(imag(eta));%Evanecent inverter
  gazx=exp(2*pi*i*dt*eta);
  phiout(j,:)=spec(j,:).*gazx;
end
clear eta; clear gazx; clear spec;
%********************************
%***restore the dip limited spectrum to full size***
kxs=1+(cins-cs)/2;
out=zeros(rins,cins);
out(:,kxs:kxs+cs-1)=phiout;
%***************************************************