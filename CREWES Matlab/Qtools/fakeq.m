function [Q,Qrand]=fakeq(vp,rho,Qmin,Qmax,delQpct,vp0,vp1,rho0,rho1,nv,nr)
% FAKEQ: invents a Q log given sonic and density logs
%
% [Q,Qrand]=fakeq(vp,rho,Qmin,Qmax,delQpct,vp0,vp1,rho0,rho1,nv,nr)
%
%Idea: map velocity to Q via a linear relation. The linear relation is
%defined by specifying two points of the mapping: Qmin to vp0 and Qmax to
%vp1. This then determines the relation:
%   Qv=Qmin*((vp-vp0)/(vp1-vp0)).^nv+Qmax*((vp-vp1)/(vp0-vp1)).^nv
% A similar map of density to Q is determined by the points: Qmin to rho0
% and qmax to rho. This gives the density relation
%   Qr=Qmin*((rho-rho0)/(rho1-rho0)).^nr+Qmax*((rho-rho1)/(rho0-rho1)).^nr
% Then the velocity Q and density Q are combined into a final Q given by
%   1./Q = 1./Qv + 1./Qr = (Qv + Qr)./(Qv.*Qr);
% Once this final Q is calculated, a random fluctuation is added of
% magnitude Qfluct=Q*delQpct/100; Then Q=Q+Qfluct gives the final Q.
%
% WARNING: This is entirely an empirical hunch. No claim of physical
% accuracy is made or implied.
%
% vp ... input velocity 
% rho ... input density
% ***** rho and vp must be vectors of the same size ******
% Qmin ... minimum allowed Q
% Qmax ... maximum allowed Q
% delQpct ... standard deviation of random Q fluctuations, expressed as a
%       percentage (a number between 0 and 100)
% vp0 ... low-end reference velocity
% vp1 ... high end reference velocity
% rho0 ... low-end reference density
% rho1 ... high-end reference density
% nv ... velocity exponent, 1 means linear interpolation, 2 quadratic etc
% ********* default =1 **********
% nr ... density exponent, 1 means linear, 2 quadratic
% ********* default =1 **********
%
% Q ... estimated Q vector (deterministic)
% Qrand ... estimated Q with random fluctuations
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

if(size(vp)~=size(rho))
    error('vp and rho must be the same size');
end
if(nargin<11)
    nr=1;
end
if(nargin<10)
    nv=1;
end

Qv=Qmin*((vp-vp1)/(vp0-vp1)).^nv+Qmax*((vp-vp0)/(vp1-vp0)).^nv;
%search for negative Q's
ind=find(Qv<Qmin);
if(~isempty(ind))
    Qv(ind)=Qmin;
end

Qr=Qmin*((rho-rho1)/(rho0-rho1)).^nr+Qmax*((rho-rho0)/(rho1-rho0)).^nr;
%search for negative Q's
ind=find(Qr<Qmin);
if(~isempty(ind))
    Qr(ind)=Qmin;
end

Q= 2*Qv.*Qr./(Qv+Qr);



%include random fluctuations
Qfluct=randn(size(Q)).*Q*delQpct/100;

Qrand=Q+Qfluct;