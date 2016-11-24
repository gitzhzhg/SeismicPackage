function refl=zoeppritz_reflections(vp,vs,rho,angles)
% Compute amplitude versus angle of incidence of PP reflections from Vp, Vs, and rho
% using the Zoeppritz equation;
% Matrices "vp", "vs", and "rho" must have the same dimensions. Also, they must 
% have at least two rows.
%
% Written by: E. R.: November 19, 2007
% Last updated: December 18, 2007: new Zoeppritz function
%
%       refl=zoeppritz_reflections(vp,vs,rho,angles)
% INPUT
% vp    column vector or matrix of P-velocities
% vs    column vector or matrix of S-velocities
% rho   column vector or matrix of densities
% angles   row vector of angles of incidence (in degrees)
% OUTPUT
% refl  matrix of reflection coefficients; 
%       if "vp" etc. are vectors then "refl" is a matric with one column per angle; 
%       size(refl) = [length(vp)-1,length(angles)]
%       if "vp" etc. are matrices then "refl" is a three-dimensional matrix 
%       with the last dimension representing the angle
%       size(refl) = [size(vp,1)-1,size(vp,2),length(angles)]

nangles=length(angles);
[temp,ntr]=size(vp);
nrefl=temp-1;

%       Handle case of ntr==1 separately to avoid singleton dimension in 
%       the middle of a three-dimensional array
if ntr == 1
   refl=NaN(nrefl,nangles);
   for ii=1:nrefl
      refl(ii,:)=zoeppritz_solid_solid(vp(ii:ii+1),vs(ii:ii+1),rho(ii:ii+1),angles,'P',[1 0 0 0],'complex');
   end

else
   refl=NaN(nrefl,ntr,nangles);
   for jj=1:ntr
      for ii=1:nrefl
         refl(ii,jj,:)=zoeppritz_solid_solid(vp(ii:ii+1,jj),vs(ii:ii+1,jj),rho(ii:ii+1,jj),angles,'P',[1 0 0 0],'complex');
      end
   end
end
