function [spec,f,kx]=fktran_mc(seis,t,x,ntpad,nxpad,percent,ishift)
% [spec,f,kx]=fktran_mc(seis,t,x,ntpad,nxpad,percent,ishift)
%
% FKTRAN_MC is logically equivalent to FKTRAN. The functional difference
% is that the MC version uses far less memory but is slower. FKTRAN needs
% enough memory to hold 2 arrays simultaneously. The MC version needs only
% slightly more than one array. Like FKTRAN, it
% uses Matlab's built in fft to perform a 2-d f-k transform
% on a real valued (presumably seismic time-space) matrix. Only the
% positive f's are calculated while all kxs are. The inverse transform
% is performed by ifktran.
%
% seis ... input 2-d seismic matrix. One trace per column.
% t ... vector of time coordinates for seis. length(t) must be the
%	same as number of rows in seis.
% x ... vector of space coordinates for seis. length(x) must be the same
% 	as the number of columns in seis.
% ntpad ... pad seis with zero filled rows until it is this size.
%	******* default = next power of 2 ******
% nxpad ... pad seis with zero filled columns until it is this size.
%   ******* default = next power of 2 ******
% percent ... apply a raised cosine taper to both t and x prior to zero pad.
%	length of taper is theis percentage of the length of the x and t axes
%   ******* default = 0 *********
% ishift ... if 1, then the k axis of the transform is unwrapped to put
%	kx=0 in the middle.
%   ******* default = 1 *******
% spec ... complex valued f-k transform of seis
% f ... vector of frequency coordinates for the rows of spec
% kx ... vector of wavenumber coordinates for the columns of spec
% 
% G.F. Margrave, CREWES Project, U of Calgary, 1996
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
[nsamp,ntr]=size(seis);
if(length(t)~=nsamp)
	error(' Time coordinate vector is incorrect');
end
if(length(x)~=ntr)
	error(' Space coordinate vector is incorrect');
end
if(nargin<7) ishift=1; end
if(nargin<6) percent=0.; end
if(nargin<5) nxpad=2^nextpow2(x); end
if(nargin<4) ntpad=2^nextpow2(t); end
%use fftrl for the t-f transform
for k=1:length(x)
	[tmp,f]=fftrl(seis(:,k),t,percent,ntpad);
	if( k == 1)
		spec=zeros(length(tmp),ntr) +i*zeros(length(tmp),ntr);
	end
	spec(:,k)=tmp;
end
clear seis;
% ok make x taper
if(percent>0)
	mw=mwindow(ntr,percent);
	%mw=mw(ones(1,ntr),:);
	%specfx=specfx.*mw;
	%clear mw;
else
	mw=ones(1,ntr);
end
nf=length(f);
ntrout=ntr;
if(ntr<nxpad)
	ntrout=nxpad;
	spec= [spec zeros(nf,nxpad-ntr)];
end
%fft on rows
for k=1:nf
	spec(k,:) = ifft(spec(k,1:ntr).*mw,ntrout);
end
% compute kx
kxnyq = 1./(2.*(x(2)-x(1)));
dkx = 2.*kxnyq/ntrout;
kx=[0:dkx:kxnyq -kxnyq+dkx:dkx:-dkx];
if(ishift==1)
	[kx,ikx]=sort(kx);
	spec=spec(:,ikx);
end	