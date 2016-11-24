function [phs,tphs]=tvconstphase(s1,s2,t,twin,tinc,flag)
% TVCONSTPHASE: estimates apparent temporally local constant phase rotations 
%
% [phs,tphs]=tvconstphase(s1,s2,t,twin,tinc,flag)
% 
% The seismic trace s1 is localized in time with a Gaussian window and then
% the constant phase rotation which best matches the localized trace to s2
% (with the same window applied) is computed (see constphase). This process
% is repeated until all specified times are analyzed.
%
% s1= input trace to be analyzed
% s2= reference trace. Constant phase rotations are w.r.t. this trace
% t= time coordinate vector for s1
% twin= width (seconds) of the Gaussian window (standard deviation)
% tinc= temporal shift (seconds) between windows
% flag ... 1 means impose the bandwidth of s1 on s2 before determining
%       rotation (done independently for each window) (see bandwidth_xfer)
%          0 means don't do that
% ************* default = 1 ***********
% phs= apparent constant phase rotations in each window
% tphs= window center times. Same size as phs
%
% NOTE: To rotate s1 to look like s2, use s1r=tvphsrot(s1,t,phs,tphs);
%
% by G.F. Margrave, Sept 2005-2016
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
    flag=1;
end

if(length(s1)~=length(s2))
    error('s1 and s2 must have the same length');
end
if(length(t)~=length(s1))
    error('s1 and t must have the same length');
end

tmin=t(1);
t=t(:);
% determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
tmax=t(end);
nwin=(tmax-tmin)/tinc+1; %this will generally be fractional
nwin=round(nwin);
tinc=(tmax-tmin)/(nwin-1); %redefine tinc
tphs=zeros(nwin,1);
phs=tphs;
for k=1:nwin
    %build the gaussian
    tnot=(k-1)*tinc+tmin;
    tphs(k)=tnot;
    gwin=exp(-((t-tnot)/twin).^2)/(sqrt(pi)*twin/tinc);
    %window and measure phase
    s1w=s1.*gwin;
    s2w=s2.*gwin;
    phs(k)=constphase2(s1w,s2w,flag);
end
