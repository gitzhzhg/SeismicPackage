function vint=vrms2vint(vrms,t,flag)
% VRMS2VINT: convert rms to interval velocity
%
% vint=vrms2vint(vrms,t,flag)
% vint=vrms2vint(vrms,t)
%
% flag=0 ... return nonphysical interval velocities as NaN
%     =1 ... interpolate interval velocities from neighbors to
%            replace non-physical results
%  ******* default = 0 **********
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

if(nargin<3)
	flag=0;
end

%force column vectors
vrms=vrms(:);
t=t(:);

%compute interval velocity squared
vint=zeros(size(vrms));
nt=length(t);
i1=1:nt-1;i2=2:nt;
vrms2=vrms.^2;
vint(i1)= (vrms2(i2).*(t(i2)-t(1))-vrms2(i1).*(t(i1)-t(1)))./(t(i2)-t(i1));
%find and process non-physical ones
ind=find(vint<0);
if(~isempty(ind))
	if(flag)
		ilive=find(vint>0);
		vint(ind)=interpextrap(t(ilive),vint(ilive),t(ind));
	else
		vint(ind)=nan*ones(size(ind));
	end
end
%compute interval velocity
vint=sqrt(vint);
vint(nt)=vint(nt-1);