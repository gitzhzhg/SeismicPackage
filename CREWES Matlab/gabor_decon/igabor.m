function [trout,t]=igabor(tvs,trow,fcol,twin,tinc,p,gdb,normflag,pow2option)
% IGABOR: inverse Gabor transform
%
% [trout,t]=igabor(tvs,trow,fcol,twin,tinc,p,gdb,normflag,pow2option)
% 
% IGABOR performs an inverse Gabor transform of a Gabor spectrum. This is
% implemented with a modified Gaussian synthesis window of unity. This is
% the inverse of fgabor. Parameters with the same name as those in fagabor
% must have exactly the same values here for a proper inverse.
%
% tvs ... input time variant spectrum or Gabor spectrum. This is typically
%      created by FGABOR.
% trow ... time coordinate vector for the rows of tvs
% fcol ... frequency coordinate vector for the columns of tvs
% twin ... half-width (seconds) of the Gaussian window
% tinc ... temporal shift (seconds) between windows
% p ... exponent used in analysis windowing. If g is the modified Gaussian
%   window then the synthesis window actually used is g.^(1-p) . 
%   The value of p must lie in the interval [0,1]. 
% ************** default = 1 ***************
% gdb ... number of decibels below 1 at which to truncate the Gaussian
%   windows. This should be a positive number. Making this larger
%   increases the size of the Gabor transform but gives marginally better
%   performance.
% ************** default = 60 ***************
% ************** the gdb feature is presently disabled **********
% normflag ... 1 means divide each window by the computed normalization
%   factor, 0 means don't divide.
% ************** default =1 ***************
% pow2option ... 1 means expand each window to a power of two in length
%            ... 0 don't expand
% ************** default = 1 *************
%
% NOTE: It is essential to use the same windowing parameters, including the
%   p and gdb values in IGABOR as in FGABOR.
% trout ... output time series
% t ... time coordinate vector for trout
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

if(nargin<6); p=1; end
if(nargin<7); gdb=60; end
if(nargin<8); normflag=1; end
if(nargin<9); pow2option=1; end
if(gdb<0)
    gdb=abs(gdb);
end
%disable gdb
gdb=inf;
%the following if block is here in case some old code calls fgabor. Formerly,
%the 6th agrument was "padflag" which is not not used. So, we convert
%either possibility for padflag into default for gdb
if(gdb==1 | gdb ==0)
    gdb=60;
end
small=1.e-10;
%test tinc
% if(abs(tinc-(trow(2)-trow(1)))>small)
%     error('tinc and trow are not consistent');
% end

[nwin,nf]=size(tvs);

dt=1/(2*fcol(end));%time sample rate
nt=2*(nf-1);%number of samples in the Gabor window
%determine Gabor window length T
tmax=trow(end);%maximum record time
tmin=trow(1);%minimum record time
ntsignal=round((tmax-tmin)/dt) + 1;%this should be an integer or there's a problem
t=(0:ntsignal-1)*dt+tmin;

%build first window and POU norm factor
tnot=tmin;
if(normflag==0)
    norm_factor=ones(size(t));
else
    norm_factor=0;
end
[gwin,norm_factor,tnotvec,nwin,i1]=gaussian_upou(t,tnot,twin,tinc,0,0,gdb,pow2option);
%note that tinc as returned from gaussian_upou will generally be changed.
%This is done to make sure that the first and last windows are centered on
%the first and last samples respectivly.

%test: tnotvec and trow should be identical
test=sum(abs(trow-tnotvec));
if(test>100*eps)
    error('trow is inconsistent with Gabor parameters');
end

q=1-p;
norm_factor=[];
trout=zeros(ntsignal,1);
for k=1:nwin
    [tmp,norm_factor,tnotvec,nwin,i1]=gaussian_upou(t,tnotvec(k),...
            twin,tinc,norm_factor,tnotvec,gdb,pow2option);
    if(q==0)
        gwin=ones(size(tmp));
    else 
        gwin=tmp.^q;
    end
    %iuse=i1:i1+nt-1;
    iuse=i1:length(tmp);
    %all windows have been adjusted by gaussian_upou to have the same
    %length
    tmptrace=ifftrl(tvs(k,:),fcol);%the windowed trace
    if(length(tmptrace)~=length(trout))
        trout=zeros(length(tmptrace),1);
    end
    trout(iuse)=trout(iuse)+(gwin.*tmptrace)';

%     tnot=tnot+tinc;
end
t=(0:length(trout)-1)*dt;