function [tvs,tout,fout,normf_tout,gwinmatrix]=fgabor(trin,t,twin,tinc,p,gdb,normflag,pow2option)
% FGABOR: forward Gabor transform with Gaussian analysis windowing
%
% [tvs,tout,fout,normf_tout]=fgabor(trin,t,twin,tinc,p,gdb,normflag,pow2option)
% 
% FGABOR performs a forward Gabor transform of a seismic trace using a
% modified Gaussian analysis window. (See GAUSSIAN_UPOU for a discussion.)
% The transform is implemented by windowing the 
% trace multiple times with a temporally shifted (modified) Gaussian window.  
% An ordinary fft is performed over each window. The output is a 2D matrix,
% called tvs, with the row coordinate being the time of the center of each
% Gaussian window and the column coordinate being the Fourier frequency in
% Hz. This tvs, or time variant spectrum, is also called the Gabor spectrum.
% The Gabor spectrum may be inverted with either IGABOR or IGABOR_SYN.
%
% input:
% trin ... input trace 
% t ... time coordinate vector for trin
% twin ... half-width (seconds) of the Gaussian window
% tinc ... temporal shift (seconds) between windows
% p ... exponent used in analysis windowing. If g is the modified Gaussian
%   window then the analysis window actually used is g.^p . 
%   The value of p must lie in the interval [0,1].
% ************** default p=1 ************************
% gdb ... number of decibels below 1 at which to truncate the Gaussian
%   windows. This should be a positive number. Making this larger
%   increases the size of the Gabor transform but gives marginally better
%   performance. Avoid values smaller than 20. Note that many gdb values
%   will result in identical windows if the truncated windows are
%   expanded to be a power of 2 in length.
% ************** default = 60 ***************
% ************** the gdb feature is presently disabled **********
% NOTE: It is essential to use the same windowing parameters, including the
%   p and gdb values in IGABOR as in FGABOR.
% normflag ... 1 means divide each window by the computed normalization
%   factor, 0 means don't divide.
% ************** default =1 ***************
% pow2option ... 1 means expand each window to a power of two in length
%            ... 0 don't expand
% pow2option also causes the trace is also expanded to the next power of two
% ************** default = 1 *************
%
% output:
% tvs ... output complex-valued time-variant spectrum (Gabor spectrum)
% tout ... column vector giving the row coordinate of tvs
% fout ... row vector giving the column coordinate of tvs
% normf_tout ... Gabor normalization values at window center times (tout)
%
% by G.F. Margrave, May 2001 updated July 2009, July 2011, Feb 2013
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
if(nargin<5); p=1; end
if(nargin<6); gdb=60; end
if(nargin<7); normflag=1; end
if(nargin<8); pow2option=1; end
if(gdb<0)
    gdb=abs(gdb);
end
%disable gdb
gdb=inf;
%the following if block is here in case some old code calls fgabor. Formerly,
%the 6th agrument was "padflag" which is not not used. So, we convert
%either possibility for padflag into default for gdb
if(gdb==1 || gdb ==0)
    gdb=60;
end

if(~between(1,0,p,2))
    error('p must lie in the interval [0,1]')
end
tmin=t(1);
dt=t(2)-t(1);
%make sure we have row vectors
[m,n]=size(trin);
if(n==1); trin=trin.'; end
[m,n]=size(t);
if(n==1); t=t'; end
n=length(t);
%pad trace
if(pow2option)
    trin=padpow2(trin);
    t=(0:length(trin)-1)*dt+tmin;
end
 
%build first window and POU norm factor
tnot=tmin;
if(normflag==0)
    norm_factor=ones(size(t));
else
    norm_factor=0;
end
[gwin,norm_factor,tnotvec,nwin,i1]=gaussian_upou(t,tnot,twin,tinc,norm_factor,0,gdb,pow2option);
%note that tnotvec as returned from gaussian_upou will be the allowed
%window center times for this POU.
%The first and last windows are centered at min(t) and max(t), and all
%other windows are centered on samples.

nt=length(gwin);

%Gaussian_upou computes the window length defined by gdb and then expands
%this such that the length is a power of 2. This expansion includes very
%small window values and is effectivly a zero pad. All windows have this
%nominal length and windows near the beginning or end of the interval
%actually have a larger zero pad.

% disp(['Trace length = ' num2str(n) 'samples.' ...
%     'Gabor window length = ' num2str(nt) 'samples']) 
tout=tnotvec;
%loop over windows

if(nargout>4)
    gwinmatrix=zeros(nwin,length(t));
end

for k=1:nwin  
    %apply window exponent
    if(p<1)
        if(p==0)
            gwin=ones(size(trin));
        else
            gwin=gwin.^p;
        end
    end
    
    %determine samples within the truncation limits of the window
    iuse=i1:i1+nt-1;
    if(nargout>4)
        gwinmatrix(k,iuse)=gwin;
    end
    %window and fft
    if(k==1)
        [tmp,fout]=fftrl(gwin.*trin(iuse),t(iuse));
        %in the above statement, the use of pad ensures that each windowed
        %segment has the same number of samples and hence the same
        %frequency sample rate.
        tvs=zeros(nwin,length(tmp));
        tvs(k,:)=tmp;
    else
        tvs(k,:)=fftrl(gwin.*trin(iuse),t(iuse));
    end
    %build the next gaussian
    if(k<nwin)
        tnot=tnotvec(k+1);
        [gwin,norm_factor,tnotvec,nwin,i1]=....
            gaussian_upou(t,tnot,twin,tinc,norm_factor,tnotvec,gdb,pow2option);
    end
end

%get norm_factor at window center times
normf_tout=interp1(t,norm_factor,tout);
normf_tout=normf_tout(:);

% if(nargout>3)
%     %generate sample window in the center of the trace
%     tnot=round(nwin/2)*tinc;
%     [gwin,norm_factor,tnotvec,nwin,i1]=....
%             gaussian_upou(t,tnot,twin,tinc,norm_factor,tnotvec,gdb,pow2option);
% end