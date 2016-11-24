function [Vp_eff,Vs_eff]=crack_KT(Dens1,Vp1,Vs1,PHI,alpha,Dens2,Vp2,Vs2,option)
% This is a fracture model program based on the Kuster-Toksoz's model, which
% simulates fracture by spheroidal inclusion. Multiple scattering is
% neglected in this model.
%
% Input (parameters for matrix and inclusions):
% dens1,Vp1,Vs1: density and velocity of uncracked medium.
% dens2,Vp2,Vs2: density and velocity of inclusion.
% PHI: concentration of inclusion;
% alpha: aspect ration of inclusion.
% option: inclusion shape, 0: spherical
%                          1: spheroidal.
%
% The first four input parameters are required, and the last four have
% default values, which consider the inclusion to be saline, and the
% aspect ratio, alpha, to be 0.01;
%
% Output (velocities of effective media):
% Vp_eff: P-velocity of cracked medium;
% Vs_eff: S-velocity of cracked medium;
%
% Reference
% 1."Velocity and attenuation of seismic waves in two-phase media:
% part I. Theoretical formulations" by G.T., Kuster and M.N., Toksoz, 1974.
% 2."The rock physics handbook" by G., Mavko, T., Mukerji, and J., Dvorkin .
%***************************************
%
% Zimin Zhang, CREWES Project, U of Calgary, 2008
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

% define parameters for crack
if nargin < 9, option = 1; end   % define default inclusion shape
if nargin < 8, Vs2 = 0; end      % define default inclusion S-velocity
if nargin < 7, Vp2 = 1.43; end    % define default inclusion P-velocity
if nargin < 6, Dens2 = 1.1; end  % define default inclusion density
if nargin < 5, alpha = 0.01; end % aspect ratio of inclusion
if nargin < 4, PHI = 0.01; end   % concentration of inclusion

K1=Dens1*Vp1^2-4*Dens1*Vs1^2/3;
Mu1=Dens1*Vs1^2;
c=PHI;  % inclusion concentration

%===========================================
% calculate moduli for dry inclusion
%===========================================
K2=0;
Mu2=0;
if (option == 0)
    % spherical inclusion
    Dens_eff=Dens1+c*(Dens2-Dens1);
    K_dry=(K1*(3*K2+4*Mu1)+4*c*Mu1*(K2-K1))/(3*K2+4*Mu1-3*c*(K2-K1));
    Mu_dry=Mu1*((9*K1+8*Mu1)*(Mu1*(1-c)+c*Mu2)+6*Mu2*(K1+2*Mu1))/...
        (6*(K1+2*Mu1)*(Mu2*(1-c)+c*Mu1)+Mu1*(9*K1+8*Mu1));
elseif (option == 1)
% spheroidal inclusion
    Dens_eff=Dens1*(1-c)+c*Dens2;
    % -- calculate Tiijj and Tijij ---
    A=Mu2/Mu1-1;
    B=(K2/K1-Mu2/Mu1)/3;
    R=3*Mu1/(3*K1+4*Mu1);
    phi=alpha*(acos(alpha)-alpha*sqrt(1-alpha^2))/...
        power(1-alpha^2,3/2);
    g=alpha^2*(3*phi-2)/(1-alpha^2);
    F1=1+A*(3*(g+phi)/2-R*(3*g/2+2.5*phi-4/3));
    F2=1+A*(1+3*(g+phi)/2-R*(3*g+5*phi)/2)+B*(3-4*R)+...
        0.5*A*(A+3*B)*(3-4*R)*(g+phi-R*(g-phi+2*phi^2));
    F3=1+0.5*A*(R*(2-phi)+(1+alpha^2)*g*(R-1)/alpha^2);
    F4=1+0.25*A*(3*phi+g-R*(g-phi));
    F5=A*(R*(g+phi-4/3)-g)+B*phi*(3-4*R);
    F6=1+A*(1+g-R*(g+phi))+B*(1-phi)*(3-4*R);
    F7=2+0.25*A*(9*phi+3*g-R*(5*phi+3*g))+B*phi*(3-4*R);
    F8=A*(1-2*R+0.5*g*(R-1)+0.5*phi*(5*R-3))+B*(1-phi)*(3-4*R);
    F9=A*(g*(R-1)-R*phi)+B*phi*(3-4*R);
    % calculate dry effective moduli
    K_dry=(4*Mu1*c*(F1/F2)*(K2-K1)+K1*(3*K1+4*Mu1))/...
        (3*K1+4*Mu1-3*c*(F1/F2)*(K2-K1));
    N=2/F3+1/F4+(F4*F5+F6*F7-F8*F9)/(F2*F4);
    Mu_dry=Mu1*(c*N*(Mu2-Mu1)*(9*K1+8*Mu1)+25*Mu1*(3*K1+4*Mu1))/...
        (25*Mu1*(3*K1+4*Mu1)-6*(K1+2*Mu1)*c*N*(Mu2-Mu1));
else
    error('this is not an option! option should be "0" or "1".');
end

% calculate effective velocity by Gassmann fluid substitution
Ki=Dens2*Vp2^2-4*Dens2*Vs2^2/3;
Mu_i=Dens2*Vs2^2;
K_eff=K1*(K_dry/(K1-K_dry)+Ki/(c*(K1-Ki)))/...
    (1+K_dry/(K1-K_dry)+Ki/(PHI*(K1-Ki)));
Mu_eff=Mu_dry;
% calculate effective velocity
Vp_eff=sqrt((K_eff+4*Mu_eff/3)/Dens_eff);
Vs_eff=sqrt(Mu_eff/Dens_eff);