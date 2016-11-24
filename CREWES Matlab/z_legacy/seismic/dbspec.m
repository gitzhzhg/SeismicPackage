function hs=dbspec(t,s,flag,n)
% dbspec(t,s,flag,n)
% dbspec(t,s,flag)
% dbspec(t,s)
%
% DBSPEC opens up a new figure window and plots a simple Fourier
% decibel spectrum (unwrapped).
% If s is a vector, then a single curve is
% plotted showing the amplitude spectrum of s. If s is a matrix,
% then a separate spectral curve is determined and plotted for each
% row of s. (The decibel reference is max(max(S)) where S is the
% amplitude spectrum of s.)
%
% Note: DBSPEC uses PLOT for display and calls SIMPLEZOOM 
% to provide an interactive Zoom
%
% s= input trace
% t= input time vector
% flag= 0... apply an n length mwindow to s before transforming
%       1... apply an n length half-mwindow to s before transforming
%       2... transform directly without windowing
% ************* default=2 **************
% n= transform length, if not supplied, then fft length
%    will be length(s)
% 
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
figure 
[m,nn]=size(s);
if(nargin<4)
   n=length(s);
 end
if nargin<3
   flag=2;
end
if m>nn, s=s.'; end
% test to see if s is complex
test=imag(s);
mag=sum(abs(test));
% branch accordingly
if(mag>0.0)
   if flag <2
      mw=ones(size(s(1,:)));
      if flag==0, mw=mwindow(s(1,:),20);end
      if flag==1, mw=mwhalf(s(1,:),20);end
      for it=1:m, s(it,:)=s(it,:).*mw;end
    end
    spec=abs(fftshiftm(fft(s.',n)));
    db=20*log10(spec/max(max(spec)));
    dt=t(2)-t(1);
    fn=1./(2*dt);
    df=1/((n-1)*dt);
    f=linspace(-fn,fn-df,n);
 else
    if flag <2
      mw=ones(size(s(1,:)));
      if flag==0, mw=mwindow(s(1,:),20);end
      if flag==1, mw=mwhalf(s(1,:),20);end
      for it=1:m, s(it,:)=s(it,:).*mw;end
    end
    s=abs(fft(s',n));
    spec=s(1:round(n/2),:);
    db=20*log10(spec/max(max(spec)));
    dt=t(2)-t(1);
    fn=1./(2*dt);
    f=linspace(0.,fn,round(n/2));
 end
% now plot it
   hs=plot(f,db);
   xlabel('Frequency (Hz)');
   ylabel(' db down');
	  grid;
	  %set(gca,'xlabel',text(0,0,' Frequency (Hz)'));
	  %set(gca,'ylabel',text(0,0,' db down'));
   simplezoom 