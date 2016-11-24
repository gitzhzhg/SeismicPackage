function stackg=gabordecon_stack(stack,t,twin,tinc,tsmo,fsmo,ihyp,stab,phase)
% GABORDECON_STACK: applies gabor decon to a stacked section
%
% stackg=gabordecon_stack(stack,t,twin,tinc,tsmo,fsmo,ihyp,stab,phase)
%
% GABORDECON_STACK applies gabordecon to all traces in a
% stack. 
%
% stack ... stacked section as a matrix of traces. 
% t ... time coordinate for stack
% twin ... half width of gaussian temporal window (sec)
% tinc ... temporal increment between windows (sec)
% tsmo ... size of temporal smoother (sec)
% fsmo ... size of frequency smoother (Hz)
% ihyp ... 1 for hyperbolic smoothing, 0 for ordinary boxcar smoothing
%    Hyperbolic smoothing averages the gabor magnitude spectrum along
%    curves of t*f=constant.
% ************** Default = 1 ***********
% stab ... stability constant
%   ************* Default = 0 **************
% phase ... 0 for zero phase, 1 for minimum phase
%   ************* Default = 1 **************
% 
% stackg ... deconvolved stack
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

%check for 3D
sz=size(stack);
threeD=0;
if(length(sz)==3)
    threeD=1;
end


if(threeD)
    disp('GABORDECON in 3D')
    
    nxlines=sz(2);
    nilines=sz(3);
    nt=sz(1);
    
    if(length(t)~=nt)
        error('invalid t coordinate vector')
    end
    
    stackg=zeros(nt,nxlines,nilines);
    
    small=100*eps;
    
    t0=clock;
    ievery=1;
    for kx=1:nxlines
        for ki=1:nilines
            tmp=stack(:,kx,ki);
            if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
                tmp2=gabordecon(tmp,t,twin,tinc,tsmo,fsmo,ihyp,stab,phase);
                stackg(:,kx,ki)=tmp2;
            end
        end
        if(rem(kx,ievery)==0)
            tnow=clock;
            time_used=etime(tnow,t0);
            time_per_xline=time_used/kx;
            time_remaining=(nxlines-kx)*time_per_xline;
            disp(['finished xline ' int2str(kx) ' of ' int2str(nxlines)])
            disp(['time used ' int2str(time_used/60) ' min, which is ' num2str(time_used/3600) ' hrs']);
            disp(['estimated time remaining ' int2str(time_remaining/60) ' min, which is ' num2str(time_remaining/3600) ' hrs'])
            disp([])
        end
    end
else
    
    
    ntr=size(stack,2);
    
    if(length(t)~=size(stack,1))
        error('invalid t coordinate vector')
    end
    
    stackg=zeros(size(stack));
    
    small=100*eps;
    
    t0=clock;
    ievery=50;
    for k=1:ntr
        tmp=stack(:,k);
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
            stackg(:,k)=gabordecon(stack(:,k),t,twin,tinc,tsmo,fsmo,ihyp,stab,phase);
        end
        if(rem(k,ievery)==0)
            tnow=clock;
            time_used=etime(tnow,t0);
            time_per_trace=time_used/k;
            time_remaining=(ntr-k)*time_per_trace;
            disp(['finished trace ' int2str(k) ' of ' int2str(ntr)])
            disp(['estimated time remaining ' int2str(time_remaining) ' sec'])
        end
    end
    
end
