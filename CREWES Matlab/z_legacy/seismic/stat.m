function [trout,tout]=stat(trin,t,dt,flag)
% [trout,tout]=stat(trin,t,dt,flag)
% [trout,tout]=stat(trin,t,dt)
%
% STAT time shifts a time series by dt seconds.
%   A sample at time t on input will be at t+dt on output
%
% trin= tinput trace
% t= time coordinate vector for trin
% dt = static shift in seconds
% flag = 0 ... the time coordinate of trout will equal that of trin
%      = 1 ... the time coordinate of trout will be
%               0<= tout <= max(t)+dt ... dt>0.0
%               -dt <= tout <= max(t) ... dt<0.0
%   ************** default = 0 *************
%
% trout= time shifted output trace
%
% by G.F. Margrave, July, 1991
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
if nargin<4, flag=0; end
% pad trin
 lt= length(trin);
 nt= abs(round(dt/(t(2)-t(1))));
 while length(trin)<lt+nt, trin=padpow2(trin,1); end
 t=xcoord(t(1),t(2)-t(1),trin);
% fft
 [Trin,f]=fftrl(trin,t);
% phase shift
 shiftr=exp(-i*2.*pi*dt*f);
 Trout=Trin.*shiftr;
% inverse fft
 trout=ifftrl(Trout,f);
%
 if flag==0,
  trout=trout(1:lt);
  tout=t;
 else
  if dt>=0,
    trout=trout(1:lt+nt);
    tout=xcoord(0.,t(2)-t(1),trout);
  else
    lt2=length(trout);
    trout=[trout(lt2-nt+1:lt2) trout(1:lt)];
    tout=xcoord((-nt+1)*(t(2)-t(1)),t(2)-t(1),trout);
  end
 end