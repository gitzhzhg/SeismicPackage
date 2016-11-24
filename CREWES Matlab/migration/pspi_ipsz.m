function phiout=pspi_ipsz(phiin,f,dx,parms,pspi_parms,dz,zj,zt)
%
% phiout=pspi_ipsz(phiin,f,dx,parms,pspi_parms,dz,zj,zt);
%
% Isotropic pspi extrapolation with topography. Adapted from pspi_ips
%
% phiout...f-x spectrum* of extrapolated wavefield.
% phiin...f-kx spectrum* of input wavefield.
% f...frequency axis (Hz).
% dx...trace spacing (m).
% parms...velocity (m/s).
% pspi_parms...low-pass filtered** velocity (m/s).
% dz...depth distance through which to extrapolate.
%
% zj...scalar depth of interest (the j-th depth times the "dz" increment)
% zt...Topography which is a Vector with surface elevations for each 
%    receiver location.
%   MUST BE a vector the same length as size(parms,2)
%
% stride...%1/2/dx/stride is the spatial Nyquist of the low-pass filter.
%
% *Please see calling function 'pspi_zero_mig.m' for details of 'spectrum'.
%
% R. J. Ferguson, 2009
%
% Included topography handling by S. Guevara, 2011
% Theory in Al-Saleh, Margrave and Gray, 2009. 
% Base code name: pspi_ips
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
pspi_parms=pspi_parms(:);
[rparms cparms]=size(parms);
%*************************

%***check input***
if rf~=rp;error('  frequency axis incorrect');end
%*****************

%***initialize some variables***
kx=ones(rp,1)*fftshift(1/2/cp/dx*[-cp:2:cp-2]);%wavenumbers (uncentered).
k=f(:)*(1./parms');
k0=f(:)*(1./pspi_parms');
%*******************************
% To eliminate the infinite values of k and k0 (when vel=0)
for i=1:rp,
    for j=1:cp,
        if isinf(k(i,j)),k(i,j)=0;end
        if isinf(k0(i,j)),k0(i,j)=0;end
    end
end

%***extrapolate one dz***
vref=unique_vels(pspi_parms);
[rv,cv]=size(vref(:));

temp=fft(phiin,[],2); % Initialyzing

% Definitions of extrapolation depth ranges: 
%               1: The extrapolation depth zj+dz is above the Topography.
%               2: Topograhy is below zj but above zj+dz
%               3: Topography is above zj.
hj1=find(zt>=(zj));          % air
hj2=find((zt>zj-dz)&(zt<(zj))); % inside
hj3=find(zt<=zj-dz);               % under   

for j=1:rv    % Selecting Vels
	if vref(j)~=0,  % Zero velocity excluded 
    inds=find(pspi_parms==vref(j));
 
    temp1=zeros(rp,cp);
    % Selecting ranges    
    ind1=intersect(inds,hj1);
    ind2=intersect(inds,hj2);
    ind3=intersect(inds,hj3);

    temp1(:,ind1)=temp(:,ind1); % No variation in the wavefield over h(x)
	temp2=fft(ips(phiin,f,dx,vref(j),(dz/2)),[],2);
    temp1(:,ind2)=temp2(:,ind2); % Wavefield about h(t): with dz/2
	temp3=fft(ips(phiin,f,dx,vref(j),dz),[],2);
    temp1(:,ind3)=temp3(:,ind3);

    temp(:,inds)=temp1(:,inds);
    end
end

thin_lens=exp(2*pi*1i*dz*(k-k0));
thin_lens2=exp(2*pi*1i*(dz/2)*(k-k0));
thin_lens(:,hj1)=1;
thin_lens(:,hj2)=thin_lens2(:,hj2);
phiout=thin_lens.*temp;
%************************