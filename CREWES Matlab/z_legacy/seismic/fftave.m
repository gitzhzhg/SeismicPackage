function [spec,f]= fftave(s,t,percent,n)
% [spec,f]= fftrl(s,t,percent,n)
% [spec,f]= fftrl(s,t,percent)
% [spec,f]= fftrl(s,t)
%
% Forward fourier transform of a real trace. This is done in brute
% force fashion to mimic the result of Vern Herbert's real to
% complex FFT. A penalty of a factor of two is paid. Relative to
% MATLAB's fft it returns only the positive frequencies in an array
% half the size. FFTRL is ensemble capable.
%
% s= input trace 
% t= input time vector
% percent= specifies the length of a raised coisine taper to be
%          applied to s prior to any padding. Taper is a percent
%          of the length of s. Taper is applied using "mwindow"
%          from the seismic toolbox. Default=0%
% n= length to which the input trace is to be padded with zeros.   
% spec= output spectrum
% f= output frequency sample vector
%
% by G.F. Margrave, May 1991
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
% set defaults
 if(nargin<4)
			n=length(s);
	end
 if(nargin<3)
   percent=0.0;
	end
% determine number of traces in ensemble
 ntraces=min(size(s));
 nsamps=max(size(s));
 [l,m]=size(s);
 if l>m, s=s.'; end
% apply the taper
 if(percent>0)
  mw=mwindow(nsamps,percent);
  for k=1:ntraces,
   s(k,:)=s(k,:).*mw;
  end
 end
% pad s if needed
 if length(s)<n,
  s=[s zeros(ntraces,n-length(s))];
  nsamps=n; 
 end
% transform the array, This is done in a loop to conserve memory
 spec=zeros(1,nsamps/2+1);
 for k=1:ntraces,
     temp=fft(s(k,:));
     spec=spec + abs(temp(1:round(n/2+1)));% save only the positive frequencies
 end
% build the frequency vector
 spec = spec/ntraces;
 fnyq=1./(2*(t(2)-t(1)));
 f=linspace(0.,fnyq,length(spec));