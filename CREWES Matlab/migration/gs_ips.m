function phiout=gs_ips(phiin,f,dx,parms,dz)
%phiout=gs_ips(phiin,f,dx,parms,dz)
%
%Isotropic generalized phase extraploation.
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
%*Please see calling function 'gs_zero_mig.m' for details of 'spectrum'.
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
[rp cp]=size(phiin);
f=f(:);
[rf cf]=size(f);
parms=parms(:);
[rparms cparms]=size(parms);
%*************************

%***check input***
if rf~=rp;error('  frequency axis incorrect');end
%*****************

%***initialize some variables***
kx=ones(rp,1)*fftshift(1/2/cp/dx*[-cp:2:cp-2]);%wavenumbers (uncentered).
k=f(:)*(1./parms');
k0=f(:)*(1./(.99*min(parms)))*ones(1,cp);
%*******************************

%***extrapolate one dz***
kz0=sqrt(k0.^2-kx.^2);
kz0=real(kz0)+sign(dz)*i*abs(imag(kz0));
pass=find(real(kz0));
focus=exp(2*pi*i*dz*kz0);
thin_lens=exp(2*pi*i*dz*(k-k0));

W0=thin_lens.*fft(phiin,[],2);
w0=ifft(W0,[],2);

lam1=-1/2*(k0.^2-k.^2);
w1=ifft(2*pi*i*dz*lam1.*W0,[],2);
kap1=zeros(rf,cp);
kap1(pass)=kz0(pass).^(-1)-k0(pass).^(-1);
t1=w1./w0.*kap1;

lam2=1/8*(k0.^2-k.^2).^2;
w2=ifft(2*pi*i*dz*lam2.*W0,[],2);
kap2=zeros(rf,cp);
kap2(pass)=kz0(pass).^(-3)-k0(pass).^(-3);
t2=w2./w0.*kap2;

lam3=-1/16*(k0.^2-k.^2).^3;
w3=ifft(2*pi*i*dz*lam3.*W0,[],2);
kap3=zeros(rf,cp);
kap3(pass)=kz0(pass).^(-5)-k0(pass).^(-5);
t3=w3./w0.*kap3;

lam4=5/128*(k0.^2-k.^2).^4;
w4=ifft(2*pi*i*dz*lam4.*W0,[],2);
kap4=zeros(rf,cp);
kap4(pass)=kz0(pass).^(-7)-k0(pass).^(-7);
t4=w4./w0.*kap4;

q=imag(t1+t2+t3+t4);
p=real(t1+t2+t3+t4);
a=1+(p./(1+i*q));
phiout=focus.*w0.*exp(i*q)./abs(a).*a;
%************************