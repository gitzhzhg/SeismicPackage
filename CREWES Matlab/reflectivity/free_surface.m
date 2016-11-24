function[rp] = free_surface(a,b,SSlowness,uuC2,uuC,uC,wC,zs,alpha,beta)

%This routine computes the free-surface boundary conditions for 
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

%incident P and SV waves. 

% vertical slowness of the first layer
am = a(1);
bm = b(1);
a = alpha(1);
b= beta(1);
amI = complex(-imag(am),real(am));
bmI = complex(-imag(bm),real(bm));

% auxiliar quantities
aux1 = SSlowness(1) - uuC2;
aux12 = aux1 * aux1;
ambm = am * bm;
ambm4uu = 4 * ambm * uuC;

den = aux12 + ambm4uu;
den = 1 / den;   

% the coefficients
auxm1 = ambm4uu - aux12;
rp(1,1) = auxm1 * den;          % Rpp

auxm1 = bm * aux1;
auxm3 = 4 * auxm1 * uC;
auxm3 = auxm3*b/a;
rp(1,2) = den * auxm3;          % Rsp

auxm1 = am * aux1;
auxm3 = 4 * auxm1 * uC;
auxm3 = auxm3*a/b;
rp(2,1) = den * auxm3;          % Rps

rp(2,2) = -1.0*rp(1,1);             % Rss

% the phase shift matrix above the source (Muller (1985), Equation (23))
wThick = -  2 * wC * zs;
E(1,1) = exp(amI * wThick);
E(1,2) = exp((amI + bmI) * wThick * 0.5);
E(2,1) = E(1,2);
E(2,2) = exp(bmI * wThick);

% applying phase-shift (Muller (1985), Equation (22))
rp = rp .* E;