function Qint=qz2qint(Q,vp,z,z1,z2,f1,f0)
%
% Qint=qz2qint(Q,vp,z1,z2,f1,f0)
%
% Given a Q vesus z function, calculate the interval Q between any two
% depths. The velocity information is needed to compute traveltimes and the
% frequency information is needed to account for the frequency dependent
% phase velocities
%
% Q ... vector of Q values
% vp ... vector of phase velocities
% z ... vector of depths
% z1 ... depth to the top of the interval of interest
% z2 ... depth to the bottom of the interval of interest
% f1 ... frequency of interest. This should be the dominant frequency of
%       seismic data
% f0 ... dominant frequency of measurement for the phase velocities. The
%       default is appropriate if the vp come from sonic logging and are
%       not check shot corrected.
% **************** default 12500 Hz ****************
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

if(nargin<7)
    f0=12500;
end
if(length(Q)~=length(vp))
    error('Q and vp must be the same length')
end
if(length(z)~=length(Q))
    error('Q and z must be the same length')
end
if(z1<z(1))
    error('z1 cannot be less than z(1)')
end
if(z2>z(end))
    error('z2 cannot be greater than z(2)')
end

Q=Q(:);
vp=vp(:);
z=z(:);

%determine if z1 and z2 are present in z. If they are not present, then we
%insert them.
ind1=find(z>=z1);
if(z(ind1(1))~=z1)
    %ok, insert z1 in layer ind1(1)-1
    z=[z(1:ind1(1)-1); z1; z(ind1(1):end)];
    Q=[Q(1:ind1(1)-1); Q(ind1(1)-1); Q(ind1(1):end)];
    vp=[vp(1:ind1(1)-1); vp(ind1(1)-1); vp(ind1(1):end)];
end
ind2=find(z>=z2);
if(z(ind2(1))~=z2)
    %ok, insert z2 in layer ind2(1)-1
    z=[z(1:ind2(1)-1); z2; z(ind2(1):end)];
    Q=[Q(1:ind2(1)-1); Q(ind2(1)-1); Q(ind2(1):end)];
    vp=[vp(1:ind2(1)-1); vp(ind2(1)-1); vp(ind2(1):end)];
end

%compute travel times at f0
t0=vint2t(vp,z);

%compute drift time
tdr=tdrift(Q,z,f1,vp,f0);
%traveltime at f1
t1=t0+tdr;

%find indices of zone of interest
inzone=near(z,z1,z2);
dtk=diff(t1(inzone));%time thicknesses of layers
Qint=sum(dtk)/sum(dtk./Q(inzone(1:end-1)));%the last Q value is not used