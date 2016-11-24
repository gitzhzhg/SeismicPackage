function [refl,coeff]=ava_approximation_rueger(vp,vs,rho,epsilon,delta,angles)
% Compute approximate amplitude vs angle of incidence from Vp, Vs, density
% and anisotropy parameters using A. Rueger's formula (Geophysics, 1997, p. 713 ff)
% if these elastic parameters are vectors they must have the same length.
%
% Written by: E. Rietsch: April 18, 2005
% Last updated:
%
%         [refl,coeff]=ava_approximation_rueger(vp,vs,rho,epsilon,delta,angles)
% INPUT
% vp      column vector of P-velocities
% vs      column vector of S-velocities
% rho     column vector of densities
% epsilon anisotropy parameter
% delta   anisotropy parameter
% angles   row vector of angles of incidence (in degrees)
% OUTPUT
% refl  matrix of reflectivities; one column per angle
%       size(refl,1) = size(vp,1)-1
% coeff  coefficients of the approximation a*f1(theta)+b*f2(theta)+c*f3(theta)

temp=rho.*vp;
r0=diff(temp)./(temp(1:end-1,:) + temp(2:end,:));

temp=rho.*(vs.^2);
dg=diff(temp)./(temp(1:end-1,:) + temp(2:end,:));

vpbar=vp(1:end-1,:) + vp(2:end,:);
vsbar=vs(1:end-1,:) + vs(2:end,:);

rvp=diff(vp)./vpbar;

g=rvp - 4*(vsbar./vpbar).^2.*dg + 0.5*diff(delta);

g1=rvp + 0.5*diff(epsilon);

ang=pi*angles/180;
sinang2=sin(ang).^2;

% refl=r0(:,ones(size(ang))) + (g + g1*tan(ang).^2).*sin(ang).^2;
refl=r0(:,ones(size(ang))) + g*sinang2 + g1*(sinang2.*tan(ang).^2);

coeff=[r0,g,g1];
