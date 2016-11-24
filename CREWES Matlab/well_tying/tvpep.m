function pep=tvpep(s,sp,t,twin,tinc)
% TVPEP: time-variant measurement of portion energy predicted (PEP) (used to assess well
% ties)
%
% pep=tvpep(s,sp,t,twin,tinc)
% 
% PEP, or portion energy predicted, is a measurment of the quality of a synthetic seismogram
% compared to a real trace. Usually the seismogram, sp, is created as a convolution with an
% estimated wavelet and the reflectivity measured at the well, while the real trace, s, is from a
% final section at a location near the well. PEP is defined as pep=1-sum((s-sp).^2)/sum(s.^2)).
% The term being subtracted from 1 is the energy of the residual trace (s-sp) divided by the
% energy of the trace. To make this process time-variant, a Gaussian window is defined, with a
% given width and center time, and applied to both s and sp befor the calculation. Repeating
% this for a complete set of window locations spaiing the length of the trace completes the
% process.
%
% s ... input real seismic trace
% sp ... input synthetic seismogram
% t ... time coordinate for both s and sp
% NOTE: s,sp, and t must all be identically sized column vectors. This usually means that s
% must be shortened before calling this function.
% twin ... half-width of Gaussian window in seconds.
% *********** default (t(end)-t(1))/10 **********
% tinc ... increment between adjacent windows in seconds. Note that tinc should be a good deal
%       less that twin.
% *********** default twin/4 ***********
%
% pep ... output pep vector. It has mesurements every tinc seconds but has been interpolated to
%       be the same size as t.
%
% G.F. Margrave, Devon Canada, 2016
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

if(nargin<4)
    twin=(t(end)-t(1))/10;
end
if(nargin<5)
    tinc=twin/4;
end

if(length(s)~=length(sp))
    error('s and sp must have the same length');
end
if(length(t)~=length(s))
    error('s and t must have the same length');
end

tmin=t(1);
t=t(:);
s=s(:);
sp=sp(:);
% determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
tmax=t(end);
nwin=(tmax-tmin)/tinc+1; %this will generally be fractional
nwin=round(nwin);
tinc=(tmax-tmin)/(nwin-1); %redefine tinc
tpep=zeros(nwin,1);
pep2=zeros(nwin,1);
nt=length(t);
dt=t(2)-t(1);
n2=round(2*twin/dt);
nuse=-n2:n2;
for k=1:nwin
    %build the gaussian
    tnot=(k-1)*tinc+tmin;
    inot=round(tnot/dt)+1;
    iuse=nuse+inot;
    if(iuse(1)<1)
        iuse=1:iuse(end);
    end
    if(iuse(end)>nt)
        iuse=iuse(1):nt;
    end
    tpep(k)=tnot;
    gwin=exp(-((t(iuse)-tnot)/twin).^2);
    %window and measure correlation
    sw=s(iuse).*gwin;
    spw=sp(iuse).*gwin;
    [tmp,a]=lsqsubtract(sw,spw);
    spw=a*spw;
    pep2(k)=1-sum((sw-spw).^2)/sum(sw.^2);
end

pep=interp1(tpep,pep2,t);