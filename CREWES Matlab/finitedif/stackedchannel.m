function [x,z,vp,rho,I]=stackedchannel(dx,xmax,zmax)
% this function creates a stacked channel model with a gradient overburden.
% [x,z,vp,rho,I]=stackedchannel(dx)
%
% dx=grid size;
%
% figure;
% imagesc(x,z,vp);colorbar
%
% by Heather Lloyd
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
if(nargin==0)
    [x,z,vp,rho,I]=stackedchannel(1);
    figure
    imagesc(x,z,vp);colorbar
    title('Stackedchannel model, colors indicate velocity')
    xlabel('distance (m)')
    zlabel('depth (m)')
    
    clear;
    return
end
if(nargin<2)
    xmax=3000;
end
if(nargin<3)
    zmax=1000;
end

z=0:dx:zmax;
x=0:dx:xmax;
obd=600;
chanshft=600;
chscl=.15;
% Velocity Model :
vp=3800*ones((round(zmax/dx)+1),(round(xmax/dx)+1));
vp(1:near(z,obd),:)=linspace(2000,3800,near(z,obd))'*ones(1,length(x));
vp=afd_vmodel(dx,vp,3500,[428.07943      1411.8164      2524.8893      2246.6211      1968.3529      1690.0846      1411.8164      1153.1445      919.94792      674.01367      428.07943],chanshft+chscl*[904.28571      932.43697      864.87395      1046.6561      1167.6589      1238.6139      1270.2521      1237.8782      1157.0246      1002.4926      904.28571]);
vp=afd_vmodel(dx,vp,3500,[992.45443      1972.2721      3089.2643      2810.9961      2532.7279      2254.4596      1976.1914      1717.5195      1484.3229      1238.3887      992.45443],chanshft+chscl*[617.1429      628.4034      577.7311      759.5132      880.5161       951.471      983.1092      950.7353      869.8817      715.3498      617.1429]);

%Density Model :
rho=2600*ones((round(zmax/dx)+1),(round(xmax/dx)+1));
rho(1:near(z,obd),:)=linspace(1250,2600,near(z,obd))'*ones(1,length(x));
rho=afd_vmodel(dx,rho,2400,[428.07943      1411.8164      2524.8893      2246.6211      1968.3529      1690.0846      1411.8164      1153.1445      919.94792      674.01367      428.07943],chanshft+chscl*[904.28571      932.43697      864.87395      1046.6561      1167.6589      1238.6139      1270.2521      1237.8782      1157.0246      1002.4926      904.28571]);
rho=afd_vmodel(dx,rho,2400,[992.45443      1972.2721      3089.2643      2810.9961      2532.7279      2254.4596      1976.1914      1717.5195      1484.3229      1238.3887      992.45443],chanshft+chscl*[617.1429      628.4034      577.7311      759.5132      880.5161       951.471      983.1092      950.7353      869.8817      715.3498      617.1429]);

I=rho.*vp;