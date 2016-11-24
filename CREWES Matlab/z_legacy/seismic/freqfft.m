function f=freqfft(t,n,flag) 
 
% f=freqfft(t,n,flag) 
% f=freqfft(t,[],flag)
% f=freqfft(t);
%
% returns the appropriate frequency coordinate vector for a 
% time series, s, whose spectrum, S, was calculated with
% S= fftshift(fft(s,n)) (sort of, see flag parameter)
%
% t= input time coordinate vector corresponding to s
% n= padded length of fft 
% ******** default n=length(t) ***********
% flag ... if 0, the f vector is returned unwrapped (f=0 in center)
%          if 1, the f vector is returned wrapped (f=0 is first sample)
%   ******* default 0 *******
% f= output frequency vector 
%
% by G.F. Margrave, 2003-2005
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

% set default
 if nargin==1
  n=length(t);
 end
 if(isempty(n))
     n=length(t);
 end
%
if(nargin<3)
    flag=0;
end
 fnyq=.5/(t(2)-t(1));
 %df=2.*fnyq/n;
 dt=t(2)-t(1);
 df=1/(n*dt);
 if(floor(n/2)*2==n)
    %even case
    f1=(0:n/2)*df;%positive f's
    f=[-fliplr(f1(2:end)) f1(1:end-1)];
else
    %odd case
    f1=df*(0:(n-1)/2); %positive f's
    f=[-fliplr(f1(2:end)) f1];
end
if(flag)
    ind=find(f==0);
    f=[f(ind:end) 2*fnyq+f(1:ind-1)];
end