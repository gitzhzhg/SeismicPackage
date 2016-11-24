function [trout,tout]=stat(trin,t,dt,flag)
% STAT: static shift a trace
% [trout,tout]=stat(trin,t,dt,flag)
% [trout,tout]=stat(trin,t,dt)
%
% STAT time shifts a time series by dt seconds.
%   A sample at time t on input will be at t+dt on output
%
% trin= tinput trace or trace gather (if a gather, then one trace per
%       column)
% t= time coordinate vector for trin
% dt = static shift in seconds (either a single value or one entry per trace)
% flag = 0 ... the time coordinate of trout will equal that of trin
%               (means the shifted trace loses samples off the beginning or
%               the end)
%      = 1 ... the time coordinate of trout will be
%               0<= tout <= max(t)+dt ... dt>0.0
%               -dt <= tout <= max(t) ... dt<0.0
%   (the shifted trace grows in size and retains all input samples)
%   ************** default = 0 *************
% NOTE: if a gather is input, then only flag=0 is implemented
%
% trout= time shifted output trace
% tout = time coordinate of output trace
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
[nr,nc]=size(trin);
ntraces=1;
if((nr-1)*(nc-1)~=0)
    ntraces=nc;
%     error('STAT can handle only one trace at a time');
end
if(nr==1)
   trin=trin(:);%force column vector
end
if(ntraces>1)
    %gather
    if(length(dt)==1)
        dt=dt*ones(1,ntraces);
    elseif(length(dt)~=ntraces)
        error('there must be one static per trace')
    else
        dt=dt(:)';%ensure a row vector
    end
    % pad trin
    nt=size(trin,1);%original length
      %lt= length(trin);
    nstat= abs(ceil(dt/(t(2)-t(1))));
    tmp=trin(:,1);
    while length(tmp)<=nt+nstat, tmp=padpow2(tmp,1); end
    trin=[trin;zeros(length(tmp)-nt,ntraces)];
    t=(0:length(trin)-1)'*(t(2)-t(1))+t(1);%extend t to account for pad
    % fft
    [Trin,f]=fftrl(trin,t);
    % phase shift
    shiftr=exp(-1i*2.*pi*f*dt);
    Trout=Trin.*shiftr;
    % inverse fft
    trout=ifftrl(Trout,f);
    %
%remove pad
      trout=trout(1:nt,:);
      tout=t(1:nt);
else
    %single trace
        % pad trin
      nt=length(trin);%original length
      %lt= length(trin);
     nstat= abs(ceil(dt/(t(2)-t(1))));
     while length(trin)<=nt+nstat, trin=padpow2(trin,1); end
     t=(0:length(trin)-1)'*(t(2)-t(1))+t(1);%extend t to account for pad
    % fft
     [Trin,f]=fftrl(trin,t);
%     Trin=fft(trin);
%     f=freqfft(t,[],1)';
    % phase shift
%
% apply a low-pass filter to reject frequencies hight than .9 of Nyquist because these 
% don't behave well. shiftr is a linear phase shift combined with a zero
% phase lowpass.
%
     shiftr=exp(-1i*2.*pi*dt*f).*filtspec(t(2)-t(1),max(t)-t(1),[0 0],[.9*max(f) .1*max(f)],0,40);
%      shiftr=exp(-1i*2.*pi*dt*f).*sigmoid_window(f,.9*max(f),2);
     Trout=Trin.*shiftr;
     Trout(end)=real(Trout(end));
    % inverse fft
     trout=ifftrl(Trout,f);
%     trout=real(ifft(Trout));
    %
     if flag==0,
      trout=trout(1:nt);
      tout=t(1:nt);
     else
      if dt>=0,
        if(nstat+nt<length(trout))
            trout=trout(1:nstat+nt);
        end
        tout=(0:length(trout)-1)'*(t(2)-t(1));
      else
        nt2=length(trout);
        trout=[trout(nt2-nstat+1:nt2);trout(1:nt)];
        tout=(0:length(trout)-1)'*(t(2)-t(1))+(-nstat+1)*(t(2)-t(1));
      end
     end
     if(nr==1)
         trout=trout';
         tout=tout';
     end
end