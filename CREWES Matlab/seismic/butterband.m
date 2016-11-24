function sf=butterband(s,t,fmin,fmax,n,phase)
% BUTTERBAND ... Butterworth bandpass, high-pass, and low-pass filtering
%
% sf=butterband(s,t,fmin,fmax,n,phase)
%
% BUTTERBAND uses Matlab's BUTTER command to design and apply a Butterworth
% filter. BUTTER is contained in the SIGNAL toolbox so this will only work
% if you have that toolbox. The filter can be either minimum or zero phase
% although it is naturally minimum phase. To generate zero phase, the
% minimum phase filter is applied forward and backward. So, effectivly the
% zero phase filter has twice the order (n) as the minimum phase. The
% filter can be either bandpass, highpass, or lowpass.
% 
% s ... seismic trace or gather (one trace per column).
% t ... time coordinate vector for s
% fmin ... filter low cut (Hz), enter 0 for low-pass filter
% fmax ... filter high cut (Hz), enter 0 for high-pass
% n ... butterworth order. Higher order means greater rejection but perhaps
%       more objectionable time-domain ripples. 
%   Recommended: 4 for zero phase, 8 for minimum phase
% ******* default n=4 ********
% phase ... 1 for minimum phase, 0 for zero phase
% ******* default =0 *********
%
% sf ... output filtered trace(s). The same size as s.
%
%
% by G.F. Margrave, March 2015
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

if(nargin<6)
    phase=0;
end
if(nargin<5)
    n=4;
end

[nr,nc]=size(s);
nt=length(t);
trunc=0;
if(nc==nt && nr==1)
    s=s.';
    trunc=1;
elseif(nc==nt && nr>1)
    error('multiple traces should be stored in columns')
elseif(nr~=nt && nc~=nt)
    error('t and s have incompatible sizes'); 
end

if(phase~=1 && phase ~=0)
    error('invalid flag');
end
dt=t(2)-t(1);
fn=.5/dt;
if(fmin>fn || fmin<0)
    error('invalid fmin specification')
end
if(fmax>fn || fmax<0)
    error('invalid fmax specification')
end

if(n<0)
    error('invalid Butterworth order')
end



W1=fmin/fn;
W2=fmax/fn;
if(W1==0)
    [B,A]=butter(n,W2,'low');
elseif(W2==0)
    [B,A]=butter(n,W1,'high');
else
    [B,A]=butter(n,[W1 W2]);
end
if(phase==0)
    sf=filtfilt(B,A,s);
elseif(phase==1)
    sf=filter(B,A,s);
end

if(trunc)
    sf=sf.';
end