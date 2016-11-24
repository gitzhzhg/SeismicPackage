function [Vp_eff,Vs_eff]=crack_Hudson(Dens,Vp,Vs,PHI,alpha,Dens2,Vp2,Vs2,option)
% This is a fracture model program based on Hudson's cracked media, assuming
% ONE SET of cracks with the normal aligned with the z-axis. The effective
% moduli of cracked media are calculated from background moduli and second-order
% corrections. The cracked media will display transverse isotropic symmetry
% with respect to the z-axis.
%
% Input:
% Dens: density of uncracked media
% Vp, Vs: P- and S-velocity of uncracked media
% PHI: crack porosity
% alpha: aspect ratio
% Dens2: density of inclusion media
% Vp2, Vs2: P- and S-velocity of inclusion media
% option = 1 effective moduli from fluid substitution for fluid filled
%            cracks;
%        = 2 effective moduli for weak inclusion (no fluid substitution);
%        = 3 dry cracks.
%
% Output:
% Vp_eff: P-velocities (Vp0: along crack normal, Vp90: along crack plane)
%           of cracked media
% Vs_eff: S-velocities (Vsv0: along crack normal, Vsv90: along crack
%           plane) of cracked media
%
% Assumption and limitations
% - Idealized crack shape (penny-shaped) with small aspect ratios and crack
%   density are assumed: crack radius and distance between cracks are much
%   smaller than a wavelength.
% - The Hudson's model is appropriate for high-frequency laboratory conditions.
%   For low-frequency field situations Brown and Korringa¡¯s relation is
%   used for fluid substitution.
%
% Reference:
% 1. Hudson, J.A., 1981. Wave speeds and attenuation of elastic waves in
%    material containing cracks. Geophys. J. Royal Astronom. Soc. 64, 133-150.
% 2. Brown, R., and Korringa, J., 1975. On the dependence of the elastic
%    properties of a porous rock on the compressibility of the pore fluid.
%    Geophysics, 40, 606-616.
%--------------------------------------------------------------------------
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
if nargin < 9, option = 1; end   % default effective moduli type
if nargin < 8, Vs2 = 0; end   % define default inclusion S-velocity
if nargin < 7, Vp2 = 1.43; end   % define default inclusion P-velocity
if nargin < 6, Dens2 = 1.1; end   % define default inclusion density
if nargin < 5, alpha = 0.01; end % aspect ratio of inclusion
if nargin < 4, PHI = 0.01; end   % concentration of inclusion

epsilon=3*PHI/(4*pi*alpha);   %crack density
% define moduli for 'weak' inclusion;
K2=Dens2*Vp2^2-4*Dens2*Vs2^2/3;  %bulk-modulus of inclusion
Mu2=Dens2*Vs2^2;   %shear modulus of inclusion
% calculate background mineral moduli
lamda=Dens*(Vp*Vp-2*Vs*Vs); %dens*(Vp*Vp-2*Vs*Vs)=K-2*Mu/3;
Mu=Dens*Vs*Vs;    %dens*Vs*Vs;

switch option;  % inclusion type
    case 1
        U1=16*(lamda+2*Mu)/(3*(3*lamda+4*Mu));
        U3=4/3*(lamda+2*Mu)/(lamda+Mu);
        Dens_eff=(1-PHI)*Dens;
    case 2
        Dens_eff=(1-PHI)*Dens+PHI*Dens2;
        M=4*Mu2*(lamda+2*Mu)/(pi*alpha*Mu*(3*lamda+4*Mu));
        kapa=(K2+4*Mu2/3)*(lamda+2*Mu)/(pi*alpha*Mu*(lamda+Mu));
        U1=(16/3*(lamda+2*Mu)/(3*lamda+4*Mu))*(1/(1+M));
        U3=(4/3*(lamda+2*Mu)/(lamda+Mu))*(1/(1+kapa));
    case 3
        U1=16*(lamda+2*Mu)/(3*(3*lamda+4*Mu));
        U3=4/3*(lamda+2*Mu)/(lamda+Mu);
        Dens_eff=(1-PHI)*Dens;
   otherwise
      error('this is not an option! option = 1, 2,or 3.');
end

%===========================================
% calculate moduli for crack media
%===========================================
%calculate the 1st-order correction
C11_1=-lamda*lamda*epsilon*U3/Mu;
C13_1=-lamda*(lamda+2*Mu)*epsilon*U3/Mu;
C33_1=-(lamda+2*Mu)*(lamda+2*Mu)*epsilon*U3/Mu;
C44_1=-Mu*epsilon*U1;
C66_1=0;
%calculate the 2nd-order correction
q=15*lamda*lamda/(Mu*Mu)+28*lamda/Mu+28;
%-------------------------------
C11_2=(q/15)*lamda*lamda*(epsilon*U3)*(epsilon*U3)/(lamda+2*Mu);
C13_2=(q/15)*lamda*(epsilon*U3)*(epsilon*U3);
C33_2=(q/15)*(lamda+2*Mu)*(epsilon*U3)*(epsilon*U3);
C44_2=(2/15)*Mu*(3*lamda+8*Mu)*(epsilon*U1)*(epsilon*U1)/(lamda+2*Mu);
C66_2=0;
%calculate background 6X6 moduli
C11=lamda+2*Mu;
C12=lamda; % C12=C11-2C66
C13=lamda;
C33=C11;
C44=Mu;
C66=C44;
% calculate effective moduli
C11_eff=C11+C11_1+C11_2;
C12_eff=C12+C11_1+C11_2;
C13_eff=C13+C13_1+C13_2;
C33_eff=C33+C33_1+C33_2;
C44_eff=C44+C44_1+C44_2;
C66_eff=C66+C66_1+C66_2;

if (option==2) | (option==3)
C2_eff=[C11_eff C12_eff C13_eff 0 0 0; ...
   C12_eff C11_eff C13_eff 0 0 0; ...
   C13_eff C13_eff C33_eff 0 0 0;...
   0 0 0 C44_eff 0 0;...
   0 0 0 0 C44_eff 0;...
   0 0 0 0 0 C66_eff];
end

if (option==1)
% Calculate the saturated moduli using Brown-Korringa fluid substituion for
% anisotropic media
% STEP 1: calculate 6X6 stiffness tensor
C20=[C11 C12 C13 0 0 0; ...
   C12 C11 C13 0 0 0; ...
   C13 C13 C33 0 0 0;...
   0 0 0 C44 0 0;...
   0 0 0 0 C44 0;...
   0 0 0 0 0 C66];
C2=[C11_eff C12_eff C13_eff 0 0 0; ...
   C12_eff C11_eff C13_eff 0 0 0; ...
   C13_eff C13_eff C33_eff 0 0 0;...
   0 0 0 C44_eff 0 0;...
   0 0 0 0 C44_eff 0;...
   0 0 0 0 0 C66_eff];
% STEP 2: calculate 3X3X3X3 stiffness tensor
for i=1:3
    for j=1:3
        if (i==1 & j==1) m=1;
        elseif (i==2 & j==2) m=2;
        elseif (i==3 & j==3) m=3;
        elseif (i==2 & j==3 | i==3 & j==2) m=4;
        elseif (i==1 & j==3 | i==3 & j==1) m=5;
        elseif (i==1 & j==2 | i==2 & j==1) m=6;
        end
        for k=1:3
            for l=1:3
                if (k==1 & l==1) n=1;
                elseif (k==2 & l==2) n=2;
                elseif (k==3 & l==3) n=3;
                elseif (k==2 & l==3 | k==3 & l==2) n=4;
                elseif (k==1 & l==3 | k==3 & l==1) n=5;
                elseif (k==1 & l==2 | k==2 & l==1) n=6;
                end
                C4(i,j,k,l)=C2(m,n);
                C40(i,j,k,l)=C20(m,n);
            end
        end
    end
end
% STEP 3: calculate 3X3X3X3 compliance tensor
S4=zeros(3,3,3,3);
S40=zeros(3,3,3,3);
for i=1:3
    for j=1:3
        for k=1:3
            for l=1:3
                for m=1:3
                    for n=1:3
                        if C4(m,n,k,l)==0
                            S4(i,j,k,l)=S4(i,j,k,l);
                        else
                            S4(i,j,k,l)=S4(i,j,k,l)+0.5*(delta(i,m)*delta(j,n)...
                              +delta(j,m)*delta(i,n))/C4(m,n,k,l);
                        end
                        if C40(m,n,k,l)==0
                            S40(i,j,k,l)=S40(i,j,k,l);
                        else
                          S40(i,j,k,l)=S40(i,j,k,l)+0.5*(delta(i,m)*delta(j,n)...
                            +delta(j,m)*delta(i,n))/C40(m,n,k,l);
                        end
                    end
                end
            end
        end
    end
end
% STEP 4: calculate 6X6 compliance tensor for dry rock
for i=1:3
    for j=1:3
        if (i==1 & j==1) m=1;
        elseif (i==2 & j==2) m=2;
        elseif (i==3 & j==3) m=3;
        elseif (i==2 & j==3 | i==3 & j==2) m=4;
        elseif (i==1 & j==3 | i==3 & j==1) m=5;
        elseif (i==1 & j==2 | i==2 & j==1) m=6;
        end
        for k=1:3
            for l=1:3
                if (k==1 & l==1) n=1;
                elseif (k==2 & l==2) n=2;
                elseif (k==3 & l==3) n=3;
                elseif (k==2 & l==3 | k==3 & l==2) n=4;
                elseif (k==1 & l==3 | k==3 & l==1) n=5;
                elseif (k==1 & l==2 | k==2 & l==1) n=6;
                end
                if (m<=3 & n<=3)
                    S2(m,n)=S4(i,j,k,l);
                    S20(m,n)=S40(i,j,k,l);
                elseif (m>=3 & n>=3)
                    S2(m,n)=S4(i,j,k,l)*4;
                    S20(m,n)=S40(i,j,k,l)*4;
                else
                    S2(m,n)=S4(i,j,k,l)*2;
                    S20(m,n)=S40(i,j,k,l)*2;
                end
            end
        end
    end
end
% STEP 5: fluid substitution using Brown-Korringa relation
% compressibility k=Siikk, (sum over i and k)
beta_fluid=1/K2;
beta0=1/(C13+2*C44/3);  % compressibility for isotropic uncracked media
beta01=0;
beta_dry=0;
for i=1:3
    for j=1:3
        beta01=beta01+S40(i,i,j,j);
        beta_dry=beta_dry+S4(i,i,j,j);
    end
end

for i=1:3
    for j=1:3
        for k=1:3
            for l=1:3
                Ssat(i,j,k,l)=S4(i,j,k,l)-...
                    (S2(i,j)-S20(i,j))*(S2(k,l)-S20(k,l))/...
                    ((beta_dry-beta0)+(beta_fluid-beta0)*PHI);
            end
        end
    end
end

C4_eff=zeros(3,3,3,3);
for i=1:3
    for j=1:3
        for k=1:3
            for l=1:3
                for m=1:3
                    for n=1:3
                        if S4(m,n,k,l)==0
                            C4_eff(i,j,k,l)=C4_eff(i,j,k,l);
                        else
                        C4_eff(i,j,k,l)=C4_eff(i,j,k,l)+0.5*(delta(i,m)*delta(j,n)...
                        +delta(j,m)*delta(i,n))/Ssat(m,n,k,l);
                        end
                    end
                end
            end
        end
    end
end
% calculate 6X6 stiffness tensor
for i=1:3
    for j=1:3
        if (i==1 & j==1) m=1;
        elseif (i==2 & j==2) m=2;
        elseif (i==3 & j==3) m=3;
        elseif (i==2 & j==3 | i==3 & j==2) m=4;
        elseif (i==1 & j==3 | i==3 & j==1) m=5;
        elseif (i==1 & j==2 | i==2 & j==1) m=6;
        end
        for k=1:3
            for l=1:3
                if (k==1 & l==1) n=1;
                elseif (k==2 & l==2) n=2;
                elseif (k==3 & l==3) n=3;
                elseif (k==2 & l==3 | k==3 & l==2) n=4;
                elseif (k==1 & l==3 | k==3 & l==1) n=5;
                elseif (k==1 & l==2 | k==2 & l==1) n=6;
                end
                C2_eff(m,n)=C4_eff(i,j,k,l);
            end
        end
    end
end
Dens_eff=(1-PHI)*Dens+PHI*Dens2;
end

% calculate effective velocity
% angle from symmetry axis - crack normals
Vp_90=sqrt(C2_eff(1,1)/Dens_eff);
Vsv_90=sqrt(C2_eff(4,4)/Dens_eff);
Vp_0=sqrt(C2_eff(3,3)/Dens_eff);
Vsv_0=sqrt(C2_eff(4,4)/Dens_eff);

Vp_eff=[Vp_0,Vp_90];
Vs_eff=[Vsv_0,Vsv_90];