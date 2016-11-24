function [seis,t,x]=ifktran_mc(spec,f,kx,nfpad,nkpad,percent)
% [seis,t,x]=ifktran_mc(spec,f,kx,nfpad,nkpad,percent)
%
% IFKTRAN_MC is the memory conserving version of IFKTRAN. 
% It uses Matlab's built in fft to perform a 2-d inverse f-k transform
% on a complex valued (presumably seismic f-k) matrix.  The forward transform
% is performed by fktran.
%
% spec ... complex valued f-k transform
% f ... vector of frequency coordinates for the rows of spec.  
%	length(f) must be the same as number of rows in spec.
% kx ... vector of wavenumber coordinates for the columns of spec
%	length(kx) must be the same as the number of columns in spec.
% nfpad ... pad spec with zero filled rows until it is this size.
%	******* default = next power of 2 ******
% nkpad ... pad spec with zero filled columns until it is this size.
%   ******* default = next power of 2 ******
% percent ... apply a raised cosine taper to both f and kx prior to zero pad.
%	length of taper is theis percentage of the length of the kx and f axes
%   ******* default = 0 *********
% seis ... output 2-d seismic matrix. One trace per column.
% t ... vector of time coordinates for seis. 
% x ... vector of space coordinates for seis. 
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
[nf,nkx]=size(spec);
if(length(f)~=nf)
	error(' Frequency coordinate vector is incorrect');
end
if(length(kx)~=nkx)
	error(' Wavenumber coordinate vector is incorrect');
end
if(nargin<6) percent=0.; end
if(nargin<5) nkpad=2^nextpow2(kx); end
if(nargin<4) nfpad=2^nextpow2(f); end
%determine if kx needs to be wrapped
if(kx(1)<0) % looks unwrapped
	ind=find(kx>=0);
	kx=kx([ind 1:ind(1)-1]);
	spec=spec(:,[ind 1:ind(1)-1]);
end
% ok taper and pad in kx
if(percent>0)
	mw=mwindow(nkx,percent);
	mw=mw([ind 1:ind(1)-1]);
	%mw=mw(ones(1,nkx),:);
	%spec=spec.*mw;
	%clear mw;
else
	mw=ones(1,nkx);
end
ntrout=nkx;
if(nkx<nkpad)
	ntrout=nkpad;
	spec=[spec zeros(nf,nkpad-nkx)];
end
%ok kx-x transform
%disp('kx-x')
for k=1:nf
	spec(k,:)= fft(spec(k,1:nkx).*mw,ntrout);
end
%use ifftrl for the f-t transform
%disp('f-t');
for k=1:ntrout
	[tmp,t]=ifftrl(spec(:,k),f);
	if(k==1)
		seis=zeros(length(tmp),ntrout);
	end
	seis(:,k)=tmp;
end
clear spec;
% compute x
dkx=kx(2)-kx(1);
xmax = 1./(dkx);
dx = xmax/nkx;
x=0:dx:xmax-dx;