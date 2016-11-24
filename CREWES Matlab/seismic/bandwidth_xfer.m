function s2p=bandwidth_xfer(s1,s2,n)
% BANDWIDTH_XFER ... transfer the bandwidth of s1 to s2
%
% s2p=bandwidth_xfer(s1,s2,n)
%
% Transfer the amplitude spectrum of s1 to s2 in a smooth way. Let A1 and
% A2 be the smoothed amplitude spectra of s1 and s2. Then the the complex
% spectrum of s2P is S2p=S2.*A1./A2, where S2 is the complex spectrum of
% S2.
%
% s1 ... first input signal
% s2 ... second input signal (must be the same length as s1). s2 can be a
%       matrix in which case each column is shaped to the spectrum of s1.
% n ... number of samples in convolutional smoother to be applied to the
%       amplitude spectra of s1 and s2.
% ********* default is 10% of the length of the spectra ************
% s2p ... the new s2 with the amplitude spectrum of s1.
%
% 
% by G.F. Margrave, Nov. 2005
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

s1=s1(:);%force column vector
if(isvector(s2))
    if(length(s1)~=length(s2))
        error('s1 and s2 must be the same length')
    end
    c2=1;r2=length(s2);
    s2=s2(:);%make sure its a column vector
else
    [r2,c2]=size(s2);
    if(r2~=length(s1))
        error('number of rows of s2 must equal the length of s1')
    end
end

% impose the bandwidth of the first signal on the second
s1p=padpow2(s1);
S1=fftshift(fft(s1p));
if(nargin<3)
    n=round(length(S1)/10);
end
n=2*floor(n/2)+1;%forces n to be odd
n1=length(S1);
AS1=zeros(size(S1));
AS1(2:n1)=convz(abs(S1(2:n1)),ones(1,n)/n);%the smoothed amplitude spectrum of s1
AS1(1)=abs(S1(1)); %this method of smoothing is done to ensure symmetry
s2p=zeros(r2,c2);

for k=1:c2
    tmp=padpow2(s2(:,k));
    S2=fftshift(fft(tmp));
    AS2=zeros(size(S2));
    AS2(2:n1)=convz(abs(S2(2:n1)),ones(1,n)/n);
    AS2(1)=abs(S2(1));
    S2p=S2.*AS1./AS2;
    tmp=real(ifft(fftshift(S2p)));
    s2p(:,k)=tmp(1:r2);
end