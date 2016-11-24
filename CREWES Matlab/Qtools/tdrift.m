function [tdr,t0,t1]=tdrift(Q,z,v0,f1,f0)
% TDRIFT: calculate the drift time
%
% [tdr,t0,t1]=tdrift(Q,z,v0,f1,f0)
%
% Sonic logging is done at roughly f0=12500Hz while seismic data typically
% has a dominant frequency (called f1) below 50 Hz. Theory predicts that
% the velocities measured by the sonic tool will be systematically faster
% than those experienced by seismic waves. This frequency dependent
% velocity effect is the "dispersion" associated with Q attenuation. The
% drift time is the traveltime difference using the velocities at f0
% compared to f1. The formula used here is based on the Aki-Richards
% equation (5.81)
% v(f1)=v(f0)*(1+(1/(pi*Q))*log(f0/f1);
% which prescibes the variation of velocity with frequency for a constant Q
% medium. If t0 are the traveltimes at f0 and t1 are the traveltimes at f1,
% then the drift time is tdr=t0-t1. This will be positive if f0>f1.
%
% Q ... Q value (may be a vector)
% z ... distance traveled (may be a vector such as from a well)
% v0 ... velocity measured by logging tool at frequency f0 (vector the same
%           size as z)
% f1 ... frequency of interest (a scalar, this should be the dominant
%           frequency of your seismic data) 
% f0 ... logging frequency (typically f1<<f0)
%   ********* default f0=12500 Hz ***********
% 
% tdr ... vector of drift times the same size as z
% t0 ... vector of traveltimes to each depth at frequency f0
% t1 ... vector of traveltimes to each depth at frequency f1
% Note: tdr is a one-way drift time. For application to surface reflection
% data this should be doubled.
%
%
% by G.F. Margrave, 2013
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

if(nargin<5)
    f0=12500;
end
if(length(Q)>1)
    if(size(Q)~=size(z));
        error('Q and Z must be the same size')
    end
end
if(size(v0)~=size(z))
    error('v0 and z must be the same size')
end
%correct the velocities
v1=v0.*((1-(1./(pi*Q))*log(f1/f0))).^(-1);
%compute times at f0
t0=vint2t(v0,z);
%compute times at f1
t1=vint2t(v1,z);
tdr=t1-t0;%will be a positive quantity if f0>f1