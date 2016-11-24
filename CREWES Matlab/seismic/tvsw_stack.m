function stackw=tvsw_stack(stack,t,tstart,aec_length,flow,fhigh,nfilt)
% TVSW_STACK: applies frequency domain decon to a stacked section
%
% stackd=tvsw_stack(stack,t,aec_length,flow,fhigh,nfilt)
%
% TVSW_STACK applies tvsw (time variant spectral whitening) to all traces in a
% stack. TVSW does time variant spectral whitening. A set of gaussian filters are
% designed to slice a frequency range in such a way that the linear sum of
% slices returns the original spectrum. Each slice is then ifft'd into the
% time domain and agc'd. Finally, the slices are summed in the time domain.
%
% stack ... stacked section as a matrix of traces. 
% t ... time coordinate for stack
% tstart ... start time. earlier times will be zero'd. This process works
%       best if the traces all have the same start time (defined as the
%       first live sample). Often a real stack will have a ragged start
%       time due to a mute. Choose a values such that all traces are live
%       at this time.
% aec_length ... length (seconds) of the aec operator (see aec.m, this is a
%       form of agc)
% flow ... lowest frequency to be whitened (Hz)
% fhigh ... highest frequency to be whitened (Hz)
% nfilt ... number of Gaussian filter slices
% NOTE: The Gaussians will be centered at the frequencies
%   f0=flow+(k-1)*fwidth where fwidth=(fhigh-flow)/(nfilt-1) and have
%   standard deviation of fwidth. This means that there will be some
%   whitening outside of flow->fhigh.
% 
% stackw ... whitened stack
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
    
    disp('TVSW in 3D')
    
    nxlines=sz(2);
    nilines=sz(3);
    nt=sz(1);
    
    if(length(t)~=nt)
        error('invalid t coordinate vector')
    end
    
    indt=near(t,t(1),tstart);
    indt2=near(t,tstart,t(end));
    %stack(ind,:)=0;
    
    stackw=zeros(nt,nxlines,nilines);
    
    small=100*eps;
    
    t0=clock;
    ievery=1;
    for kx=1:nxlines
        for ki=1:nilines
            tmp=stack(:,kx,ki);
            tmp(indt)=0;
            if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
                tmp2=tvsw(tmp,t,aec_length,flow,fhigh,nfilt);
                stackw(indt2,kx,ki)=tmp2(indt2);
            end
        end
        if(rem(kx,ievery)==0)
            tnow=clock;
            time_used=etime(tnow,t0);
            time_per_xline=time_used/kx;
            time_remaining=(nxlines-kx)*time_per_xline;
            disp(['finished xline ' int2str(kx) ' of ' int2str(nxlines)])
            disp(['estimated time remaining ' int2str(time_remaining) ' sec'])
        end
    end
else
    ntr=size(stack,2);
    
    if(length(t)~=size(stack,1))
        error('invalid t coordinate vector')
    end
    
    ind=near(t,t(1),tstart);
    stack(ind,:)=0;
    
    stackw=zeros(size(stack));
    
    small=100*eps;
    
    t0=clock;
    ievery=200;
    for k=1:ntr
        tmp=stack(:,k);
        
        if(sum(abs(tmp))>small)%avoid deconvolving a zero trace
            %         inot=find(tmp~=0);
            %         iz=1:inot(1);
            stackw(:,k)=tvsw(tmp,t,aec_length,flow,fhigh,nfilt);
            %         stack(iz,k)=0;
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
    
    stackw(ind,:)=0;
end
   
