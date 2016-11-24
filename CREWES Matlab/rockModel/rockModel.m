clear;
home;
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

% define the uncracked rock's properties
Vp1=3.026;
Vs1=1.721;
Dens1=2.133;
% define parameters for inclusions
PHI=0.005;
alpha=0.01;
Dens2=1.1;
Vp2=1.43;
Vs2=0;


[Vp_KT,Vs_KT]=crack_KT(Dens1,Vp1,Vs1,PHI,alpha,Dens2,Vp2,Vs2,1); % spheroid = 1, sphere =0
[Vp_KTB,Vs_KTB]=crack_KTB(Dens1,Vp1,Vs1,PHI,alpha,Dens2,Vp2,Vs2,4); % penny crack = 4
[Vp_Hud,Vs_Hud]=crack_Hudson(Dens1,Vp1,Vs1,PHI,alpha,Dens2,Vp2,Vs2,1); % penny crack with fluid substitution = 1