function [vr,xr]=kvdesample(v,x,dxnew)
% KVDESAMPLE: desample a velocity function in the wavenumber domain
%
% vr=kvdesample(v,x,dxnew)
%
% Reduce the number of samples in a regularly sampled space series
% by reducing the maximum wavenumber. This achieves a spatial desampling
% such that the new spatial sample interval, dxr, is related to the
% original, dx, by dxr=(n/m)*dx where n is the original number of points
% and m is the new number of points. An antialias filter is applied.
%
% v ... input velocity matrix. Space series are row vectors. Each row is
%           a different depth
% x ... space coordinate vector for v
% dxnew ... new spatial sample size
% vr ... resampled velocity matrix. Will have the same number of rows as
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

%           v but fewer columns.

[nz,nx]=size(v);
dx=x(2)-x(1);

%determine old and new nyquist
kx=freqfft(x,[],1);
dkx=kx(2)-kx(1);
n=length(x);
knyqnew=.5/dxnew;
%knyqnew=dkx*(ceil(knyqnew/dkx)+1); % get the next largest wavenumber
ind=find(abs(kx)>knyqnew);

kxnew=kx;
kxnew(ind)=[];

m=length(kxnew);
dxnew=(n/m)*dx;

vr=zeros(nz,m);
xr=(0:m-1)*dxnew+x(1);

for k=1:nz
    tmp=fft(v(k,:));
    tmp(ind)=[];
    vr(k,:)=m*ifft(tmp)/n;
end

xr=(0:m-1)*dxnew+x(1);