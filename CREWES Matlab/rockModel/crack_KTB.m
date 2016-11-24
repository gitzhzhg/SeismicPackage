function [Vp_eff,Vs_eff]=crack_KTB(Dens1,Vp1,Vs1,PHI,alpha,Dens2,Vp2,Vs2,option)
% This model is based on Berrymann's generalization of the Kuster-Toksoz model.
% Multiple scattering is neglected in this model.
%
% Input (parameters for matrix and inclusions):
% dens1,Vp1,Vs1: density and velocity of uncracked medium.
% dens2,Vp2,Vs2: density and velocity of inclusion.
% PHI: concentration of inclusion;
% alpha: aspect ratio of inclusion.
% option (inclusion shape):     1 = spheres;
%                               2 = needles;
%                               3 = disks;
%                       and     4 = penny cracks;
%
% The first four input parameters are required, and the last four have
% default values, which considere the inclusion to be saline, and the
% aspect ratio, alpha, to be 0.01;
%
% Output (velocities of effective media):
% Vp_eff: P-velocity of cracked medium;
% Vs_eff: S-velocity of cracked medium;
%
% CAUTION:
% 1. Since the cavities are isolated with respect to flow, this approach
% simulates very high frequency saturated rock behavior appropriate to
% ultrasonic laboratory conditions. At low frequencies, when there is time
% for wave-induced pore pressure increments to flow and equilibrate, it is
% better to find the effective moduli for dry cavities and then saturate
% them with the Gassmann low-frequency relations.
% 2. Each set of inclusions must be distributed randomly, and thus its
% effect is isotropic.
% 3. IMPORTANT COMMENT: c/alpha should not be too large, otherwise the
% assumption will be violated (multiple scatter can not be neglected).
%
% Reference
% 1."Velocity and attenuation of seismic waves in two-phase media:
%  part I. Theoretical formulations" by G.T., Kuster and M.N., Toksoz, 1974.
% 2."Long-wavelength propagation in compostie elastic media II. Ellipsoidal
%  inclusions" by J.G., Berryman, 1980.
% 3."The rock physics handbook" by G., Mavko, T., Mukerji, and J., Dvorkin .
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

% define (default) parameters for the model
if nargin < 9, option = 4; end   % define default inclusion shape
if nargin < 8, Vs2 = 0; end      % define default inclusion S-velocity
if nargin < 7, Vp2 = 1.43; end    % define default inclusion P-velocity
if nargin < 6, Dens2 = 1.1; end  % define default inclusion density and moduli
if nargin < 5, alpha = 0.01; end % aspect ratio of inclusion
if nargin < 4, PHI = 0.01; end   % concentration of inclusion

Mu_m=Dens1*Vs1^2; % matrix moduli
Km=Dens1*Vp1^2-4*Mu_m/3;
c=PHI;  % inclusion concentration

%===========================================
% STEP 1: CALCULATE DRY MODULI USING KUSTER-TOKSOZ MODEL FOR TWO-PHASE
% MEDIA
%===========================================
Ki=0; % dry cavities moduli
Mu_i=0;
% calculate P and Q values according to inclusion shape
beta_m=Mu_m*(3*Km+Mu_m)/(3*Km+4*Mu_m);
gamma_m=Mu_m*(3*Km+Mu_m)/(3*Km+7*Mu_m);
yita_m=(1/6)*Mu_m*(9*Km+8*Mu_m)/(Km+2*Mu_m);
yita_i=0;
switch option;  % option: inclusion shape
    case 1  % spheres
        P=(Km+4*Mu_m/3)/(Ki+4*Mu_m/3);
        Q=(Mu_m+yita_m)/(Mu_i+yita_m);
    case 2  % needles
        P=(Km+Mu_m+Mu_i/3)/(Ki+Mu_m+Mu_i/3);
        Q=(4*Mu_m/(Mu_m+Mu_i)+2*(Mu_m+gamma_m)/(Mu_i+gamma_m)+...
            (Ki+4*Mu_m/3)/(Ki+Mu_m+Mu_i/3))/5;
    case 3  % disks
        P=(Km+4*Mu_i/3)/(Ki+4*Mu_i/3);
        Q=(Mu_m+yita_i)/(Mu_i+yita_i);
    case 4  % penny cracks
        P=(Km+4*Mu_i/3)/(Ki+4*Mu_i/3+pi*alpha*beta_m);
        Q=(1+8*Mu_m/(4*Mu_i+pi*alpha*(Mu_m+2*beta_m))+...
            2*(Ki+2*(Mu_i+Mu_m)/3)/(Ki+4*Mu_i/3+pi*alpha*beta_m))/5;
    otherwise
        error('This is not an option (option=1-4)!');
end
% calculate moduli for dry crack media
K_dry=((Km+4*Mu_m/3)*Km+c*(Ki-Km)*P*4*Mu_m/3)/...
        (Km+4*Mu_m/3-c*(Ki-Km)*P);
Mu_dry=((Mu_m+yita_m)*Mu_m+c*(Mu_i-Mu_m)*Q*yita_m)/...
        (Mu_m+yita_m-c*(Mu_i-Mu_m)*Q);

%===========================================
% STEP 2: CALCULATE SATURATED MODULI USING GASSMANN EQUATION
%===========================================
Mu_i=Dens2*Vs2^2; % define inclusion moduli
Ki=Dens2*Vp2^2-4*Dens2*Vs2^2/3;
K_eff=Km*(K_dry/(Km-K_dry)+Ki/(PHI*(Km-Ki)))/...
    (1+K_dry/(Km-K_dry)+Ki/(PHI*(Km-Ki)));
Mu_eff=Mu_dry;
Dens_eff=Dens1+c*(Dens2-Dens1);
% calculate effective velocity
Vp_eff=sqrt((K_eff+4*Mu_eff/3)/Dens_eff);
Vs_eff=sqrt(Mu_eff/Dens_eff);