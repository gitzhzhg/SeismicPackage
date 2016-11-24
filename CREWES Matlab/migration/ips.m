function phiout=ips(phiin,f,dx,parms,dz)
%phiout=ips(phiin,f,dx,parms,dz)
%
%Isotropic phase shift extraploation (stationary).
%
%Please see 'Ferguson_Margrave_2005.pdf' or 'Ferguson and Margrave, 2005,
%Planned seismic imaging using explicit one-way operators, Geophysics, V70,
%NO5, S101 - S109' for details.
%
%phiout...f-kx spectrum* of extrapolated wavefield.
%phiin...f-kx spectrum* of input wavefield.
%f...frequency axis (Hz).
%dx...trace spacing (m).
%parms...velocity (m/s).
%dz...depth distance through which to extrapolate.
%
%*Please see calling function for details of 'spectrum'.
%
%R. J. Ferguson, 2009
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

%***get sizes of things***
[rp, cp]=size(phiin);
f=f(:);
rf=size(f,1);
parms=parms(:);
[rparms, cparms]=size(parms);
%*************************

%***check input***
if rparms~=1;error('  to many parmameters for isotropy');end
if cparms~=1;error('  not a stationary velocity');end
if rf~=rp;error('  frequency axis incorrect');end
%*****************

%***initialize some variables***
kx=fftshift(1/2/cp/dx*(-cp:2:cp-2));%wavenumbers (uncentered).
%*******************************

%***extrapolate one dz***
kz=sqrt((f*ones(1,cp)/parms).^2-(ones(rp,1)*kx).^2);%vertical slowness
kz=real(kz)+sign(dz)*1i*abs(imag(kz));%ensures evanescent region is complex positive
gazx=exp(2*pi*1i*dz*kz);%evenescent region will decay exponentially
phiout=phiin.*gazx;%phase shift the input spectrum
%************************