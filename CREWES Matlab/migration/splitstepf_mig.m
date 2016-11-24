function [seismig,zmig] = splitstepf_mig(seis, t, x, velmod, zv, dz, zmax, fmax)
% SPLITSTEPF_MIG: Split-step Fourier depth migration
%
% [seismig,zmig] = splitstepf_mig(seis, t, x, velmod, zv, dz, zmax, fmax)
%
% SPLITSTEPF_MIG performs a migration by Fourier phase shift 
% through a v(z) medium.
% 
% seis ... matrix of seismic data
% t ... time coordinate vector for seis (one entry per row, must be
%		regular)
% x ... x coordinate vector for seis. (One entry per column, must be
%		regular)
% velmod ... velocity model matrix. Must have the same number of columns as
% 		seis. Its z coordinate must span 0 to zmax.
% zv ... z coordinate for the velocity model
% dz ... depth step size
% zmax ... maximum depth to step to
% fmax ... maximum frequency
%
%
% G.F. Margrave, CREWES, 2000
%
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

tic

%global VELMOD XV ZV

[nsamp,ntr]=size(seis);

z=0:dz:zmax;
nz=length(z);

%forward fk
disp('fk transform')
[phi,f,kx]=fktran(seis,t,x,2^nextpow2(t),2^nextpow2(x),0,0);
%kx spectrum will be wrapped
kx2=kx.^2;
clear seis

df=f(2)-f(1);
nfmax=round(fmax/df)+1;
f1=f(2:nfmax);

f2=f1.^2;
phi=phi(2:nfmax,:);% don't bother with dc or f>fmax
nfmax=nfmax-1; %make nfmax point to fmax after tossing dc

ntrpad=size(phi,2);

disp([int2str(nz) ' depth steps'])

seismig=zeros(nz,length(x));

for iz=1:nz
	%get v
	izv=near(zv,z(iz));
	v=.5*velmod(izv,:);%exploding reflector
	vm=mean(v);
	
	%focussing phase shift
	for jk=1:length(kx)
		fev=kx(jk)*vm; %first non-evanescent f
		nfev=max([round(fev/df),1]);
		if(nfev<=nfmax)
			%compute focussing phase shift
			psf= (2*pi*dz*f1(nfev:nfmax)/vm).*(sqrt(1 - ...
					vm*vm*kx2(jk)./f2(nfev:nfmax) )-1);
	
			%apply phase shift
			phi((nfev:nfmax),jk) = phi((nfev:nfmax),jk).*exp(i*psf);
			if(nfev>1)
				phi(1:nfev-1,jk)=0;
			end
		else
			phi(:,jk)=0;
		end
	end

	% inverse fft over kx and apply static phase shift
	% also re-zero the zero pad, also image
	if(length(v)~=length(x));
		v=pwlinint(v,xv,x);
	end

	for jf=1:nfmax
		tmp= fft(phi(jf,1:length(x)));
		tmp=tmp.*exp(i*2*pi*f(jf)*dz./v);
		seismig(iz,:)=seismig(iz,:)+2*real(tmp);
		phi(jf,:)=[ifft(tmp) zeros(1,length(kx)-length(x))];
	end
	
	disp([' finished step ' int2str(iz) ' of ' int2str(nz)])
end
zmig=z;
time=toc;
disp(['finished in ' num2str(time) ' seconds'])