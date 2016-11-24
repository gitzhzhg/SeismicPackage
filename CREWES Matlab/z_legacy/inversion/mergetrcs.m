function trout=mergetrcs(trin1,trin2,t,fnot,delf,fhigh)
% trout=mergetrcs(trin1,trin2,t,fnot,delf,fhigh)
%
% MERGETRCS combine two time series under the assumption that
% trin1 is to be used dominantly below frequency fnot and
% trin2 is to be used dominantly above fnot. The algorithm
% is least squares and assumes:
%   Trout = Trin1.*B + beta*Trin2
% where caps on variables indicate their Fourier transforms,
% B is a zero phase lowpass filter which is rolls off rapidly
% for f>fnot, and beta is a scalar. B and beta are chosen to
% minimize the squared error computed from:
%  phi = sum( (Trout-Trin1).^2 )
% Thus trout has its high frequencies from trin2 but the 
% spectral combination is constrained to match trin1 as
% closely as possible. (So trin1 needs to be broadband even
% though only its low frequencies are used.)
%
% NOTE: this routine does no padding of trace lengths prior to
% fft's. If this is desired it should be done prior to running
% mergetrcs.
%
% trin1 ... input trace to provide the low frequencies
% trin2 ... input trace to provide frequencies between fnot and fhigh
% t     ... time coordinate vector for trin1 and trin2
% fnot  ... highest frequency for which trin1 contributes at full power
% delf  ... width of (1/e point) gaussian rolloff on lowpass filter B
% fhigh ... highest frequency to pass on trin2.
%
% G.F. Margrave May 1995
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
% check for row vector and transpose if needed
aa=size(trin1);
bb=size(trin2);
cc=size(t);
if aa(1)==1
	trin1=trin1';
end
if bb(1)==1
	trin2=trin2';
end
if cc(1)==1
	t=t';
end	
	
%do the FFT's
%tr1=padpow2(trin1,1);
%tr2=padpow2(trin2,1);
tr1=trin1;
tr2=trin2;
t2=xcoord(t(1),t(2)-t(1),tr1);
[Trin1,f]=fftrl(tr1,t2);
[Trin2,f]=fftrl(tr2,t2);
% find j2 etc
ij=find(f>=fnot);
j2= ij(1);
fnot=f(j2);
jhigh= near(f,fhigh);
juse = 1:jhigh;
n=length(f);
%make gaussian
g=gauss(f,fnot,delf);
g=g(:);
b=ones(length(juse),1);
b(j2:jhigh)=g(j2:jhigh);
%beta = sum(Trin1(juse).*conj(Trin2(juse))) - sum(Trin1(juse).*b.*conj(Trin2(juse)));
%beta = beta/( sum( Trin2(juse).*conj(Trin2(juse))));
juse2=1:juse(length(juse));
a=sum( Trin2(juse2).*conj(Trin2(juse2)));
bee=sum(conj(Trin1(juse2)).*b(juse2).*Trin2(juse2))+sum(Trin1(juse2).*b(juse2).*conj(Trin2(juse2)));
c=sum(Trin1(juse2).*b(juse2).*conj(Trin1(juse2).*b(juse2))) - sum( Trin1(juse2).*conj(Trin1(juse2)));
beta = (-bee + sqrt(bee^2 -4*a*c))/(2*a);
%form the output trace
Trout=zeros(size(Trin1));
Trout(1:jhigh)= Trin1(1:jhigh).*b;
Trout= Trout+beta*Trin2;
trout=ifftrl(Trout,f);
m=length(trin1);
trout=trout(1:m);