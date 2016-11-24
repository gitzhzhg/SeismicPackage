function [cc,tcc]=tvmaxcorr(s1,s2,t,twin,tinc,maxlag)
% TVMAXCORR: estimates time variant crosscorrelation of two signals
%
% [cc,tcc]=tvmaxcorr(s1,s2,t,twin,tinc,maxlag)
% 
% The traces s1 and s2 are localized in time with a Gaussian window and then
% the crosscorrelation is calculated (see maxcorr).
% This process is repeated until all specified times are analyzed.
%
% s1= input trace to be analyzed
% s2= reference trace. Constant phase rotations are w.r.t. this trace
% t= time coordinate vector for s1 and s2
% NOTE: s1,s2, and t must all be the same size
% twin= width (seconds) of the Gaussian window (standard deviation)
% tinc= temporal shift (seconds) between windows
% maxlag = maximum cc lag (seconds)
% ************* default = .2*twin ***********
%
% cc= n-by-2 matrix of crosscorrelations values. Here the row index gives
%      the time and column 1 is the maximum correlation value while column
%      2 is the lag (in samples) at which the maximum occurs. Lags are
%      relative to the window center time.
% tcc= n-by-1 vector of window center times. These are the times of the cc values 
%
% NOTE: To stretch s1 to look like s2 use 
% [cc,tcc]=tvmaxcorr(s1,s2,t,twin,tinc,maxlag);
% delt=cc(:,2)*dt;
% tstretch=interp1(tcc,delt,t);
% s1p=stretcht(s1,t,-tstretch);
%
% by G.F. Margrave, 2016, Devon Canada
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
    maxlag=.2*twin;
end

if(length(s1)~=length(s2))
    error('s1 and s2 must have the same length');
end
if(length(t)~=length(s1))
    error('s1 and t must have the same length');
end

tmin=t(1);
t=t(:);
s1=s1(:);
s2=s2(:);
% determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
tmax=t(end);
nwin=(tmax-tmin)/tinc+1; %this will generally be fractional
nwin=round(nwin);
tinc=(tmax-tmin)/(nwin-1); %redefine tinc
tcc=zeros(nwin,1);
cc=zeros(nwin,2);
nlag=round(maxlag/(t(2)-t(1)));
for k=1:nwin
    %build the gaussian
    tnot=(k-1)*tinc+tmin;
    tcc(k)=tnot;
    gwin=exp(-((t-tnot)/twin).^2);
    %window and measure correlation
    s1w=s1.*gwin;
    s2w=s2.*gwin;
    cc(k,:)=maxcorr(s1w,s2w,nlag);
end
