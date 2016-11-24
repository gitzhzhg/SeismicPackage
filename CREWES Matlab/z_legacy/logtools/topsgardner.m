function [m,a,rhonew]=topsgardner(vel,rho,t,tops)
% TOPSGARDENER defines a set of Gardner parameters that can be used to 
%     calculate a density log from a velocity log.  This algoritim 
%     defines a new set of paramters for each lithology interval as defined
%     in tops. This is meant to be applied in time but can be applied in 
%     depth as long as all input parameters are in depth. 
%
%     The output parameters can be applied using the following code:
%             rhonew=m.*vel.^a;
%
% [m,a,rhonew]=topsgardner(vel,rho,t,tops)
%
% vel    = seismic data matrix
% rho    = time vector
% t      = well log to match amplitudes to
% tops   = time corresponding to the end of the log
%
% m      = velocity - density scaling factor
% a      = velocity - density exponent factor
% rhonew = the new density vector as calculated using the topsgardner
%            parameters
%
% by  H.J.E. Lloyd December 2012
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
vel=vel(:);
rho=rho(:);
t=t(:);
tops=sort(tops(:));
if tops(1)~=0
    tops=[0;tops];
end

if tops(end)~=t(end)
    tops=[tops;t(end)];
end
tops=sort(tops(:));
tmin=t(1);
t=t-tmin;
%determine number of windows. tinc will be adjusted to make the
% last window precisely centered on tmax
a=zeros(size(t));
m=a;
for k=1:length(tops)-1
    ind=near(t,tops(k)):near(t,tops(k+1));
pp=polyfit(log(vel(ind)),log(rho(ind)),1);
aout=pp(1);
mout=mean(rho(ind)./(vel(ind).^aout));
m(ind)=mout;
a(ind)=aout;
end
rhonew=m.*vel.^a;