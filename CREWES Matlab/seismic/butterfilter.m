function sf=butterfilter(s,t,varargin)
% BUTTERFILTER ... Butterworth bandpass, high-pass, and low-pass filtering
%
% sf=butterfilter(s,t,varargin)
%
% BUTTERFILTER uses Matlab's BUTTER command to design and apply a Butterworth
% filter. BUTTER is contained in the SIGNAL toolbox so this will only work
% if you have that toolbox. The filter can be either minimum or zero phase
% although it is naturally minimum phase. To generate zero phase, the
% minimum phase filter is applied forward and backward. So, effectivly the
% zero phase filter has twice the order (n) as the minimum phase. The
% filter can be either bandpass, highpass, or lowpass. Option exists to
% automatically pad with zeros for better filter resolution.
% 
% s ... seismic trace or gather (one trace per column).
% t ... time coordinate vector for s
% Value-Name pairs that determine the filtering
% 'fmin' ... filter low cut (Hz), enter 0 for low-pass filter
%  ********** default is 0 ***************
% 'fmax' ... filter high cut (Hz), enter 0 for high-pass
%  ********** default is 0 ***************
% 'order' ... butterworth order. Higher order means greater rejection but perhaps
%       more objectionable time-domain ripples. Should be an integer
%       greater than 0.
% *********** default n=4 ****************
% 'phase' ... 1 for minimum phase, 0 for zero phase
% *********** default =1 *****************
% 'minlength' ... input signal will be padded with zeros to this length if 
%       they are shorter. The pad will be removed on output. This value is
%       specified in samples not in time.
% *********** default 512 samples ********
% 'padwith' ... allowed values are 'zeros', 'mean', 'trend'
% *********** default is 'zeros' *********
%
% sf ... output filtered trace(s). The same size as s.
%
%
% by G.F. Margrave, Decvon Canada, March 2016
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

if(nargin<3)
    disp('No filtering done because no parameters specified and the default is all pass')
    sf=s;
    return
end
nargs=length(varargin);
if(floor(nargs/2)*2~=nargs)
    error('input arguments must be name-value pairs')
end

fmin=0;
fmax=0;
order=4;
phase=1;
minlength=512;
nargs=length(varargin);
padwith='zeros';
for k=1:2:nargs
    arg=varargin{k};
    if(~ischar(arg))
        error(['additional arguments must be (string,value) pairs. Entry'...
            int2str(k) ' expected to be a string'])
    end
    recognized=0;
    if(strcmp(arg,'fmin'))
        fmin=varargin{k+1};
        recognized=1;
    end
    if(strcmp(arg,'fmax'))
        fmax=varargin{k+1};
        recognized=1;
    end
    if(strcmp(arg,'order'))
        order=round(varargin{k+1});
        if(order<0 )
            error('order must be an integer greater than 0');
        end
        recognized=1;
    end
    if(strcmp(arg,'minlength'))
        minlength=round(varargin{k+1});
        recognized=1;
    end
    if(strcmp(arg,'phase'))
        phase=varargin{k+1};
        if(phase~=0 && phase ~=1)
            error('phase must be 0 or 1');
        end
        recognized=1;
    end
    if(strcmp(arg,'padwith'))
        padwith=varargin{k+1};
        if(~strcmp(padwith,'zeros') && ~strcmp(padwith,'mean') && ~strcmp(padwith,'trend'))
            error('padwith must be ''zeros'' or ''mean'' or ''trend''');
        end
        recognized=1;
    end
    if(~recognized)
        error([arg ' is not a recognized parameter name'])
    end
end

dt=t(2)-t(1);
fn=.5/dt;
if(fmin>fn || fmin<0)
    error('invalid fmin specification (outside 0 to Nyquist)')
end
if(fmax>fn || fmax<0)
    error('invalid fmax specification (outside 0 to Nyquist)')
end

[nr,nc]=size(s);
nt=length(t);
ntraces=nc;
transpose=0;
if(nc==nt && nr==1)
    s=s.';
    ntraces=nr;
    transpose=1;
elseif(nc==nt && nr>1)
    error('multiple traces should be stored in columns')
elseif(nr~=nt && nc~=nt)
    error('t and s have incompatible sizes'); 
end

npad=0;
if(nt<minlength)
    npad=minlength-nt;
    s=[s;zeros(npad,ntraces)];
    if(strcmp(padwith,'mean'))
        for k=1:ntraces
            s(nt+1:end,k)=mean(s(1:nt,k));
        end
    elseif(strcmp(padwith,'trend'))
        t2=(1:npad)*dt+t(end);
        for k=1:ntraces
            p=polyfit(t,s(1:nt,k),1);
            s(nt+1:end,k)=polyval(p,t2);
        end
    end
end




W1=fmin/fn;
W2=fmax/fn;
if(W1==0)
    [B,A]=butter(order,W2,'low');
elseif(W2==0)
    [B,A]=butter(order,W1,'high');
else
    [B,A]=butter(order,[W1 W2]);
end
if(phase==0)
    sf=filtfilt(B,A,s);
elseif(phase==1)
    sf=filter(B,A,s);
end

if(npad>0)
  sf=sf(1:nt,:);
end
if(transpose)
    sf=sf.';
end