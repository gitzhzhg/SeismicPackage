function [xti,zti,fx,fz,sx,sz]=ti_impulse(x,z,t,matter,vp,vs,epsilon,delta,phi,ia,poly)
%[xti,zti,fx,fz,sx,sz]=ti_impulse(x,z,t,matter,vp,vs,epsilon,delta,phi,ia,poly);
%
%This function plots and returns 2-D coordinates of a migration impulse
%response for a homogeneous TI medium.
%
%  x...x coordinates of model
%  z...z coordinates of model
%  t...time of impulse
%  matter...name of material (string)
%  vp, vs, epsilon, delta...anisotropic parameters
%  phi...angle of TI symetry in degrees
%  ia...increment of angle in degrees
%  poly...polynomial order
%
%example: [xti,zti,fx,fz,sx,sz]=ti_impulse(0:20:1000,0:4:400,.1,'Weathered gypsum',1991,795,1.161,-1.075,-45,1,8);
%
%R. J. Ferguson, 2009
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

%***innitialize***
pang=[-pi/2:pi*ia/180:pi/2];
[r c]=size(pang);
strvp=num2str(vp);
strvs=num2str(vs);
streps=num2str(epsilon);
strdelta=num2str(delta);
strphi=num2str(phi);
strpoly=num2str(poly);
%*****************

%***make matricies of anisotropic parameters***
vp=vp*ones(1,c);
vs=vs*ones(1,c);
epsilon=epsilon*ones(1,c);
delta=delta*ones(1,c);
phi=pi*phi/180;
%**********************************************

%***make phase velocity***
aa=.5*(vp.^2).*(1-(vs./vp).^2);
bb=4*delta./(1-(vs./vp).^2).^2;
cc=4*epsilon.*(1-(vs./vp).^2+epsilon)./(1-(vs./vp).^2).^2;
dd=vp.^2;
ee=(vp.^2).*epsilon;
tivp=sqrt(aa.*sqrt(1+bb.*(sin(pang).^2).*(cos(pang).^2)+cc.*(sin(pang).^4))-aa+dd+ee.*(sin(pang).^2));
D=aa./vp.^2.*(sqrt(1+bb.*sin(pang).^2.*cos(pang).^2+cc.*sin(pang).^4)-1);
tivs=vs.*sqrt((tivp./vp).^2+epsilon.*sin(pang).^2.*((vp./vs).^2 - 1)-D.*((vp./vs).^2+1));
%*************************

%***make group angle***
part1=2*ee.*cos(pang).*sin(pang);
part2=.5*aa.*(2*bb.*(cos(pang).^3).*sin(pang)-2*bb.*cos(pang).*(sin(pang).^3)+4*cc.*cos(pang).*(sin(pang).^3));
part3=sqrt(1+bb.*(cos(pang).^2).*(sin(pang).^2)+cc.*sin(pang).^4);
part4=2*tivp;
dtivp=(part1+part2./part3)./part4;
gang=atan((tan(pang)+dtivp./tivp)./(1-tan(pang).*(dtivp./tivp)));
%**********************

%***compute group velocity for fast and slow iso curves***
tivg=sqrt(tivp.^2+dtivp.^2);
%*********************************************************

%***fit group angle and phase angle***
p=polyfit(gang,pang,poly);
%*************************************

%***Compute set of phase angles given group angles***
npang=polyval(p,pang);
%****************************************************

%***Compute a new vector of phase velocities and derivatives based on new phase angles***
tivp=sqrt(aa.*sqrt(1+bb.*(sin(npang).^2).*(cos(npang).^2)+cc.*(sin(npang).^4))-aa+dd+ee.*(sin(npang).^2));
part1=2*ee.*cos(npang).*sin(npang);
part2=.5*aa.*(2*bb.*(cos(npang).^3).*sin(npang)-2*bb.*cos(npang).*(sin(npang).^3)+4*cc.*cos(npang).*(sin(npang).^3));
part3=sqrt(1+bb.*(cos(npang).^2).*(sin(npang).^2)+cc.*sin(npang).^4);
part4=2*tivp;
dtivp=(part1+part2./part3)./part4;
%****************************************************************************************

%***compute group velocity***
tivg=sqrt(tivp.^2+dtivp.^2);
fvg=max(tivg);
svg=min(tivg);
%****************************

%***make TI coordinates and rotate***
vxti=[t*tivg.*sin(pang) fliplr(t*tivg.*sin(pang))];
vzti=[t*tivg.*cos(pang) -t*tivg.*cos(pang)];
temp=[cos(phi) sin(phi);-sin(phi) cos(phi)]*[vxti;vzti];
xti=temp(1,:)+x(round(length(x)/2+1));
zti=temp(2,:);
%************************************

%***make fast coordinates***
fx=[x(round(length(x)/2+1))+t*fvg*sin(pang) fliplr(x(round(length(x)/2+1))+t*fvg*sin(pang))];
fz=[t*fvg*cos(pang) -t*fvg*cos(pang)];
%************************

%***make slow coordinates***
sx=[x(round(length(x)/2+1))+t*svg*sin(pang) fliplr(x(round(length(x)/2+1))+t*svg*sin(pang))];
sz=[t*svg*cos(pang) -t*svg*cos(pang)];
%***************************

figure(1)
clf
whitefig
h=plot(xti,zti,fx,fz,sx,sz,x,zeros(size(x)),'--k');
set(h,'linewidth',2);
title(['Migration impulse responses: Homogeneous ',matter],'FontSize',14)
xlabel('Distance (m)','FontSize',14)
ylabel('Depth (m)','FontSize',14)
legend(['\alpha_0 = ',strvp,' m/s, \beta_0 = ',strvs,' m/s \newline \epsilon = ',streps,', \delta* = ',strdelta,', \theta = ',strphi,'^o'],['\alpha_{max} = ',num2str(round(fvg)),' m/s, \beta_{max} = ',num2str(round(max(tivs))),' m/s'],['\alpha_0 = ',num2str(vp(1)),' m/s'],'Recording surface')
axis('ij')
axis equal
axis([x(1) x(length(x)) -z(length(z)) z(length(z))])