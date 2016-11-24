function [stackd,d]=deconw_stack(stack,t,itr,tstart,tend,xdesign,top,stab)
% DECONW_STACK: applies Wiener decon to a stacked section
%
% [stackd,d]=deconw_stack(stack,t,itr,tstart,tend,xdesign,top,stab)
%
% DECONW_STACK applies deconw (Wiener deconvolution) to all traces in a
% stack. Either each trace can deconvolved independently or the operator
% can be designed from a specific trace and applied to the entire
% section. The deconvolution design gate is constant from trace-to-trace.
%
% stack ... stacked section as a matrix of traces. 
% t ... time coordinate for stack
% itr ... trace number to design operator on. Set this to 0 for
%       trace-by-trace operation.
% tstart ... vector of decon design window start times
% tend ... vector of decon design window end times
% xdesign ... vector of trace numbers at which tstart and tend apply
% NOTE: allowing tstart, tend, and xdesign to be same-size vectors allows the decon design
%   window to be varied along the section. This might be useful in a structural area. If no
%   variation is desired then just enter a single value for tstart and tend and set xdesign=1.
%   It only makes sense to prigram a variable design gate if you set itr=0 (trace-by-trace
%   operation).
% top ... length of decon operator in seconds
%  ****** default 0.1 ******
% NOTE: if top is a positive number less that 1 it is assumed to be an operator length in
%   seconds. If it is a number positive number between 1 and 100, then it is assumed to be a
%   percentage and the actual operator length is the specified percentage of the design window.
%   This allows the operator length to be varied in proportion to the design gate.
% stab ... decon white noise (stab) factor
%  ****** default =.001 *****
% 
% stackd ... deconvolved stack
% d ... deconvolution operator (only returned if itr~=0)
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
    stab=.001;
end
if(nargin<7)
    top=.1;
end

sz=size(stack);
if(length(sz)>2)
    error('deconw_stack is not enabled to process a 3D volumne');
end
ntr=sz(2);

if(length(t)~=size(stack,1))
    error('invalid t coordinate vector')
end
dt=t(2)-t(1);

stackd=zeros(size(stack));
small=1000*eps;
% idesign=near(t,tstart,tend);
% mw=mwindow(length(idesign),10);

if(itr==0)
    %interpolate the design gate to span the section
    x=1:size(stack,2);%trace numbers as x coordinate
    if(any(xdesign<x(1)) || any(xdesign>x(end)))
        error('xdesign contains values outside the range 1:ntraces');
    end
    [xd,isort]=sort(xdesign);
    tstart2=tstart(isort);
    tend2=tend(isort);
    ts=interpextrap(xd,tstart2,x,0);
    te=interpextrap(xd,tend2,x,0);
    d=[];%operator not returned in this mode
    n=0;
    ievery=200;
    for k=1:ntr
        tmp=stack(:,k);
        idesign=near(t,ts(k),te(k));
        if(sum(abs(tmp(idesign)))>small)%avoid deconvolving a zero trace
            if(top>1)
               nop=round((te(k)-ts(k))*top/(dt*100)); 
            else
               nop=round(top/dt); 
            end
            mw=mwindow(length(idesign),10);
            stackd(:,k)=deconw(stack(:,k),stack(idesign,k).*mw,nop,stab);
            n=n+1;
           
        end
        if(rem(k,ievery)==0)
            disp(['finished trace ', int2str(k) ' out of ', int2str(ntr)])
        end
    end

    
elseif(itr>=1 && itr<=ntr)
    idesign=near(t,tstart(1),tend(1));
    if(top>1)
        nop=round((tend(1)-tstart(1))*top/(dt*100));
    else
        nop=round(top/dt);
    end
    mw=mwindow(length(idesign),10);
    [tmp,d]=deconw(stack(:,itr),stack(idesign,itr).*mw,nop,stab);
    ievery=200;
    for k=1:ntr
        tmp=stack(:,k);
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
            stackd(:,k)=convm(tmp,d);
        end
        if(rem(k,ievery)==0)
            disp(['finished trace ', int2str(k) ' out of ', int2str(ntr)])
        end
    end
    
else
    error('invalid value for itr');
end
