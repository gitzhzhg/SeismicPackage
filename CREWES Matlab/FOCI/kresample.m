function [seisr,xr]=kresample(seis,x,dxout,kcrit)
% KRESAMPLE ... Resample a space series by zero padding in the wavenumber domain.
% 
% [seisr,xr]=kresample(seis,x,dxout)
%
% This is intended to be the inverse routine to kdesample. Data is Fourier
% transformed over columns to the wavenumber domain. There a zero pad is
% attached such that the desired new nyquist, defined by dxout, is achieved
% seis ... input seismic matrix, x increases along columns
%          rows can be t or f
% x ... x coordinate vector for seis
% dxout ... desired output sample size
% kcrit ... vector of critical wavenumbers, one per frequency.
%           if nonzero, then all wavenumbers higher than this are zeroed.
%           (an evanescent filter)
%       ******* default = zeros(size(seis,1),1) (means no action) *********
% seisr ... resampled data
% xr ... output x coordinate vector
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

if(nargin<4) kcrit=zeros(size(seis,1),1); end

if(length(kcrit)~=size(seis,1))
    error('kcrit is the wrong size')
end

if(abs(dxout-(x(2)-x(1)))<100*eps)
    seisr=seis;
    xr=x;
    return;
end

dxin=abs(x(2)-x(1));
[nf,m]=size(seis);
n=round(m*dxin/dxout);
%redefine dxout in case m*dxin/dxout is not an exact integer
dxout=m*dxin/n;

xr=(0:n-1)*dxout+x(1);
seisr=zeros(nf,n);

tmp=fft(seis,[],2);
if(sum(kcrit))
    kx=freqfft(x,[],1);
    for jf=1:size(kcrit)
        if(kcrit(jf)~=0)
            ind=find(abs(kx)>kcrit(jf));
            if(~isempty(ind))
                tmp(jf,ind)=0;
            end
        end
    end
end

if(rem(m,2)==0)
    %even case
    i1=1:m/2+1;
    i2=m/2+2:m;
else
    %odd case
    i1=1:((m+1)/2);
    i2=((m+1)/2)+1:m;
end
seisrk=zeros(nf,n);
seisrk(:,i1)=tmp(:,i1);
seisrk(:,n-length(i2)+1:n)=tmp(:,i2);

%seisr=(m/n)*ifft(seisrk,[],2);
seisr=ifft(seisrk,[],2);