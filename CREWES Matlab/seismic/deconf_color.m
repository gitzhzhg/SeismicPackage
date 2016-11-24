function [trout,specinv]= deconf_color(trin,trdsign,dt,fwid,Cparms,stab,phase)
% [trout,specinv]=deconf_color(trin,trdsign,dt,fwid,Cparms,stab,phase)
% [trout,specinv]=deconf_color(trin,trdsign,dt,fwid,Cparms,stab)
% [trout,specinv]=deconf_color(trin,trdsign,dt,fwid,Cparms)
%
% DECONF_COLOR performs a frequency domain deconvolution of the input trace
% assuming a coloured rather than a white reflectivity.  Running the
% function without any output arguments produces a diagnostic plot useful
% in choosing stab. The algorithm is (1) compute the amplitude spectrum of
% the input trace, (2) Divide the result of (2) by the color model, 
% (3) Procede as with white deconvolution.
%
% trin    ... input trace to be deconvolved
% trdsign ... input trace to be used for operator design
% dt      ... time same size or time coordinate vector for trin or trdsign
%
% NOTE: trin and trdsign can be the same trace which means that the decon
% operator is designed from the entire trace. More realisitically, trdsign
% is a subset (portion) of trin. Both trin and trdsign should have the same
% time sample size. dt can either be a scallar giving the same size of a
% time coordinate vector such that dt(2)-dt(1) gives the sample size.
%
% fwid    ... width in Hz of Gaussian smoother used to estimate wavelet.
%               This is the standard deviation of the Gaussian.
% Cparms  ... 3 element vector giving [f0, a, b] which are used the create
%               the amplitude spectral color model. The color model is
%               given by CM= a+b*s./sqrt(1+s.^2) with s=(abs(f)-f0)/f0 and f
%               being frequency. These parameters are currently determined
%               by COLORWELL2
% stab    ... stabilization factor expressed as a fraction of the
%               zero lag of the autocorrelation. This is equivalent to being
%               a fraction of the mean power.
%               ********* default= .0001 **********
% phase   ... 0 means zero phase whitening is performed
%             1 means minimum phase deconvolution is performed
%               ******* default= 1 *********
%
% trout   ... output trace which is the deconvolution of trin
% specinv ... output inverse operator spectrum. The time domain 
%               operator can be recovered by real(ifft(fftshift(specinv))).
%               The estimated wavelet is recovered by
%               real(ifft(fftshift(1./specinv)))
%
% by: G.F. Margrave, 2014
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
  if nargin < 8
   phase=1;
  end
  if nargin<7
    stab=.0001;
  end
  if(length(dt)>1)
      dt=dt(2)-dt(1);
  end
  
  % switch to column vectors
  [k,j]=size(trin);
  if(k==1); trin=trin.'; trdsign=trdsign.'; end
  
% pad input traces to power of 2
  N=length(trin);
%   Nd=length(trdsign);
  trin=padpow2(trin);
  trdsign=pad_trace(trdsign,trin);%ensure that trdsign is the same length as trin
%   Npad=length(trin);
%   %increase n to account for padding
%   n = Npad*n/N;
% generate the power spectrum (pad to length of trin)
  %m=length(trin)-length(trdsign);
  spec= fftshift(fft(trdsign));
  f=freqfft((0:length(trdsign)-1)*dt)';
  s=(abs(f)-Cparms(1))/Cparms(1);
  Cdb=Cparms(2)+Cparms(3)*s./sqrt(1+s.^2);
  CM=10.^(Cdb/10);%color model in Power decibels
  power= real(spec).^2 + imag(spec).^2;
  power=power./CM;%color correction
  power=power/max(power);%normalize to maximum of 1
% stabilize the power spectrum
  %mean_p= sum(power)/length(power);
  max_p=max(power);
  %delta_p= stab*mean_p;
  delta_p= stab*max_p;
  if(nargout==0)
     figure
     a=sqrt(power(length(power)/2:end));
     ma=max(a);
     %fake frequency axis
     nf=length(a);
     f=linspace(0,pi,nf);
     s1=sqrt(stab)*ma*ones(size(a));
     s2=10*s1;
     if(max(s2)>ma); s2=.9*ma; end
     s3=s1/10;
     plot(f,todb(a,ma),f,todb(s1,ma),f,todb(s2,ma),f,todb(s3,ma));
     legend('A(f) of trace','stab*max(A)','10*stab*max(A)','stab*max(A)/10')
     return
  end
  power=power+delta_p;
% create frequency smoother (odd number of points)
% this will be a Gaussian with half-width equal to two standard deviations
% and we take fwid to be the standard deviation
  delf=f(2)-f(1);
  n=round(fwid/delf);%Gaussian half width
  fg=delf*(-2*n:2*n)';%Make it two stddev wide
  smoother=exp(-(fg/fwid).^2);
  power2=convz(power,smoother)/sum(smoother);
% compute the minimum phase spectrum
  if phase==1
     logspec=hilbert(.5*log(power2));
     % compute the complex spectrum of the inverse operator
     specinv= exp(-conj(logspec));
  else
     specinv=power2.^(-.5);
  end
% deconvolve the input trace
  specin=fftshift(fft(trin));
  specout=specin.*specinv;
  trout=real(ifft(fftshift(specout)));
  trout=balans(trout,trin);
  
  %unpad to length of trin
  trout = pad_trace(trout,1:N);
  
  if( k==1 );
      trout =trout.'; 
  end
  