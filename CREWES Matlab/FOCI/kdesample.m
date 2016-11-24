function [seisr,xr]=kdesample(seis,x,f,pct,v,dcflag,nyqflag)
% KDESAMPLE: desample a space series in the wavenumber domain
%
% [seisr,xr]=kdesample(seis,x,f,pct,v)
%
% Reduce the number of samples in a regularly sampled space series
% by reducing the maximum wavenumber. This achieves a spatial desampling
% such that the new spatial sample interval, dxr, is related to the
% original, dx, by dxr=(n/m)*dx where n is the original number of points
% and m is the new number of points. An antialias filter is applied.
%
% seis ... input seismic matrix. Space series are row vectors. Each row is
%           a different frequency
% x ... space coordinate vector for seis
% f frequency coordinate vector for seis
% pct ... resampling is done to place the evanescent wavenumber at maximum
%           frequency at this percent of the Nyquist.
% v ... scalar value of velocity. Used to determine the evanescent boundary
%       by the formula kev=f/v .
% dcflag ... if nonzero, zero the 0 HZ samples (first row) in the resampled data (if present)
% nyqflag ... if nonzero, zero the last row of the resampled data
% 
% seisr ... resampled seismic matrix. Will have the same number of rows as
%           seis but fewer columns.
% xr ... space coordinate vector for seisr.
%
% G.F. Margrave, CREWES/POTSI 2004
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

if(pct<1) error('pct must be in percent'); end

pct=pct/100;

[nf,nx]=size(seis);
dx=x(2)-x(1);

%determine old and new nyquist
kx=freqfft(x,[],1);
dkx=kx(2)-kx(1);
n=length(x);
kev=max(f)/v;
knyqnew=kev/pct;
knyqnew=dkx*(ceil(knyqnew/dkx)+1); % get the next largest wavenumber
ind=find(abs(kx)>knyqnew);

kxnew=kx;
kxnew(ind)=[];

m=length(kxnew);
dxnew=(n/m)*dx;

seisr=zeros(nf,m);
xr=(0:m-1)*dxnew+x(1);

for k=1:nf
    tmp=fft(seis(k,:));
    tmp(ind)=[];
    kev=f(k)/v;
    indev=find(abs(kxnew)>kev);
    tmp(indev)=0;
    %seisr(k,:)=(m/n)*ifft(tmp);
    seisr(k,:)=ifft(tmp);
end
%zero DC and Nyquist
if(dcflag & ~f(1))
    seisr(1,:)=0;
end
if(nyqflag)
    seisr(end,:)=0;
end