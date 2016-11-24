function [stackd,specinv]=deconf_stack(stack,t,itr,tstart,tend,fsmo,stab,phase)
% DECONF_STACK: applies frequency domain decon to a stacked section
%
% [stackd,specinv]=deconf_stack(stack,t,itr,tstart,tend,fsmo,stab,phase)
%
% DECONF_STACK applies deconf (frequency domain deconvolution) to all traces in a
% stack. Either each trace can de ceonvolved independently or the operator
% can be designed from a specific trace and applied to the entire
% section. The deconvolution design gate is constant from trace-to-trace.
%
% stack ... stacked section as a matrix of traces. 
% t ... time coordinate for stack
% itr ... trace number to design operator on. Set this to 0 for
%       trace-by-trace operation.
% tstart ... start of the decon design window
% tend ... end of the decon design window
% fsmo ... length of frequency smoother in Hz
%  ****** default 5 ******
% stab ... decon white noise (stab) factor
%  ****** default =.001 *****
% phase ... phase of the operator, 1 for minimum, 0 for 0.
%  ****** default = 1 *******
% 
% stackd ... deconvolved stack
% specinv ... spectrum of the deconvolution operator (for trace-by-trace
%           mode, this is an average spectrum)
% The timedomain operator is d=real(ifft(fftshift(specinv)));
% The estimated wavelet is found by w=ifft(1./fft(d));
%
% G.F. Margrave, Devon Canada, May 2016
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
if(nargin<8)
    phase=1;
end
if(nargin<7)
    stab=.001;
end
if(nargin<6)
    fsmo=.1;
end

sz=size(stack);
if(length(sz)>2)
    error('deconf_stack is not enabled to process a 3D volumne');
end
ntr=sz(2);

if(length(t)~=size(stack,1))
    error('invalid t coordinate vector')
end

%determine smoother size in samples
df=1/(t(end)-t(1));
nop=round(fsmo/df);

stackd=zeros(size(stack));
small=1000*eps;
idesign=near(t,tstart,tend);
mw=mwindow(length(idesign),10);

if(itr==0)
    
    n=0;
    for k=1:ntr
        tmp=stack(:,k);
        if(sum(abs(tmp(idesign)))>small)%avoid deconvolving a zero trace
            [stackd(:,k),si]=deconf(stack(:,k),stack(idesign,k).*mw,nop,stab,phase);
            n=n+1;
            if(n==1)
                specinv=si;
            else
                specinv=specinv+si;
            end
        end
    end
    specinv=specinv/n;
    
elseif(itr>=1 && itr<=ntr)
    
    [tmp,specinv]=deconf(stack(:,itr),stack(idesign,itr).*mw,nop,stab,phase);
    d=real(ifft(fftshift(specinv)));
    for k=1:ntr
        tmp=stack(:,k);
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trac
             stackd(:,k)=convm(tmp,d);
        end
    end
    
else
    error('invalid value for itr');
end
