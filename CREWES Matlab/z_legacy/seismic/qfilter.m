function trout= qfilter(trin,t,q,delt,fmin,fmax)
% trout=qfilter(trin,t,q,delt,fmin,fmax)
% trout=qfilter(trin,t,q,delt,fmin)
% trout=qfilter(trin,t,q,delt)
% trout=qfilter(trin,t,q)
%
% QFILTER uses the SPIN algorithm (Margrave 1989) to apply a
% bandlimited constant Q (forward) filter to the input trace.
% The bandlimitation is a numerical requirement (which should not
% be pushed too much past .8*Nyquist) and is implemented as a 
% stationary minimum phase filter superimposed on the qfilter.
% To guard against temporal wraparound, a considerable time pad
% is required. The input trace is automatically lengthened to the
% next power of 2, however, if a larger pad is needed, it should
% be done externally. The output trace will have the same length
% as the original length of trin.
%
% trin= input trace
% t= input time coordinate for trin
% q= desired Q factor
% delt= temporal sample rate for the time variant q spectrum
%       ********** default=.05 sec ************
% fmin= minimum frequency to pass without minimum phase bandpass
%       attenuation 
%      ******** default= 10 ******* 
% fmax= maximum frequency to pass without minimum phase bandpass 
%       attenuation
%      ******** default= .6*Nyquist ******* 
% trout= output trace with forward qfilter applied
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
 
% Set defaults
  if nargin < 6
    fmax=.6/(2.*(t(2)-t(1)));
  end
  if nargin < 5
    fmin=10;
  end
  if nargin < 4
    delt=.05;
  end
% force input to be a column vector
  trin=trin(:);
  t=t(:);
% find first nonzero amplitude for later rescaling
  indicies=find(abs(trin)>0);
  imin=min(indicies);
% pad the input trace
  ntrout=length(trin);
  trin=padpow2(trin);
  dt=t(2)-t(1);
  t=xcoord(t(1),dt,trin);
% generate the time variant spectrum
  fnyq=1./(2*dt);
  f=linspace(0.,fnyq,length(trin)/2+1);
  [tvsq,tq]=tvsqf(delt,f,q,fmin,fmax);
% apply the qfilter
  fwidth=1./(2*(tq(2)-tq(1)));
  trout=tvfilt(trin,t,tvsq,tq,f,fwidth,fmin,fmax);
% use an mwindow to select the first ntrout samples
  mw=mwindow(2*ntrout-1);
  trout=trout(1:ntrout).*mw(ntrout:length(mw));
% normalize the result by equalizing the power of the first
%  10% of the live samples
  lnorm=round(ntrout*.1);
  a=norm(trin(imin:imin+lnorm),2);
  b=norm(trout(imin:imin+lnorm),2);
  trout=trout*(a/b);