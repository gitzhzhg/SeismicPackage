function sf=filt_hyp(s,t,t0,fmin,fmax,fmaxmax,phase,max_atten,option,twin)
% FILT_HYP ... time variant bandpass filter whose passband follows hyperbolic trajectories
%
% sf=filt_hyp(s,t,t0,fmin,fmax,fmaxmax,phase,max_atten,option,twin)
%
% In the time-frequency plane, curves of t*f=constant are hyperbolae. If
% the min and max frequencies of a bandpass filter are specified at some
% time, t0, then the they can be automatically adjusted to follow a
% hyperbolic trajectory. This gets a wider passband for t<t0 and a narrower
% one for t>t0.
%
% s ... seismic trace to be filters
% t ... time coordinate vecotr for s
% t0 ... time at which filter specs apply
% fmin ... a two element vector specifying:
%         fmin(1) : 3db down point of filter on low end (Hz)
%         fmin(2) : gaussian width on low end
%    note: if only one element is given, then fmin(2) defaults
%          to 5 Hz. Set to [0 0] for a low pass filter 
% fmax ... a two element vector specifying:
%         fmax(1) : 3db down point of filter on high end (Hz)
%         fmax(2) : gaussian width on high end
%    note: if only one element is given, then fmax(2) defaults
%          to 20% of (fnyquist-fmax(1)). Set to [0 0] for a high pass filter
% fmaxmax ... after adjusting fmax for t<to, it will not be allowed to
%           exceed this value.
%     ********** default fmaxmax = .8*fnyq *********
% phase... 0 ... zero phase filter
%          1 ... minimum phase filter
%          any other number ... constant phase rotation of that
%		   many degrees
%  ****** default = 0 ********
% note: Minimum phase filters are approximate in the sense that
%  the output from FILTF is truncated to be the same length as the
%  input. This works fine as long as the trace being filtered is
%  long compared to the impulse response of your filter. Be wary
%  of narrow band minimum phase filters on short time series. The
%  result may not be minimum phase.
% 
% max_atten= maximum attenuation in decibels
%   ******* default= 80db *********
% option ... 1 means only the high-end parameters follow a hyperbolae
%            2 means both low and high end follow hyperbolae
%   ******* default = 1 **********
% twin ... half-width in seconds of Gaussian Gabor windows to be used
%   ******* default = .1 seconds *********
%
% sf ... output filtered trace
% 
% G.F. Margrave, CREWES, 2009
%
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

fnyq=.5/(t(2)-t(1));
if(length(fmin)==1)
    fmin=[fmin 5];
end
if(length(fmax)==1)
    fmax=[fmax .1*fnyq];
end
if(nargin<6)
    fmaxmax=.8*fnyq;
end

if(nargin<7)
    phase =0;
end
if(nargin<8)
    max_atten=80;
end
if(nargin<9)
    option=1;
end
if(nargin<10)
    twin=.2;
end

tmax=max(t);

p=1;
tinc=twin/2;
%forward Gabor
[tvs,trow,fcol]=fgabor(s,t,twin,tinc,p,1);

%loop over rows in the Gabor spectrum
for k=1:length(trow)
    tmp=tvs(k,:);
    if(trow(k)~=0)
        fmx=fmax(1)*t0/trow(k);
    else
        fmx=fmaxmax;
    end
    if(fmx>fmaxmax);fmx=fmaxmax; end
    if(option==2)
        if(trow(k)~=0)
            fmn=fmin(1)*t0/trow(k);
        else
            fmn=fmin(2);
        end
    else
        fmn=fmin(1);
    end
    
    ftmp=(filtspec(t(2)-t(1),tmax,[fmn fmin(2)],[fmx fmax(2)],phase,max_atten))';
%     if(k>10)
%         disp('Sucks')
%     end
    %make sure Nyquist is real
    ftmp(end)=real(ftmp(end));
    tvs(k,:)=tmp.*ftmp;
end

sf=igabor(tvs,trow,fcol,twin,tinc,p);
sf=sf(1:length(s));