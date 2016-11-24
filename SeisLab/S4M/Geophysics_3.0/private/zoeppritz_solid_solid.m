function [coeff,aux]=zoeppritz_solid_solid(vp,vs,rho,anglesDeg,incident,emerging,output_type)		
% Compute reflection/transmission coefficients for a solid-solid interface
%
% Written by: E. R.: November 20, 2007
% Last updated:
%
%         coeff=zoeppritz_solid_solid(vp,vs,rho,anglesDeg,incident,emerging,output_type)		
% INPUT
% vp      two-element vector with P-velocity above and below the interface
% vs      two-element vector with S-velocity above and below the interface
% rho     two-element vector with density above and below the interface
% anglesDeg  Vector of angles of incidence in degrees
% incident  String indicating if the incident wave is compressional ('P' or 'p')
%         or shear ('S' or 's').
% emerging  logical vector; indicates for which wave types the coefficients
%         are requested.
%         for P-reflection coefficient:   emerging(1)=true
%         for S-reflection coefficient:   emerging(2)=true
%         for P-transmission coefficient: emerging(3)=true
%         for S-transmission coefficient: emerging(4)=true
% output_type   type of output of reflection coefficients in case there are
%         complex values; there are two possible values: 'complex' or 'polar'
%         Default: output_type='complex'
% OUTPUT
% coeff   reflection and/or transmission coefficients; this is a matrix with
%         as many columns as there are angles and as many rows as there are
%         'true" entries in logical vector "emerging".
%         If  at least one angle is greater than critical the matrix has 
%         complex entries.
%         If "output_type" is 'complex' the real part represents the real part 
%         of the reflection coefficients and the imaginary part the imaginary
%         part of the reflection coefficients.
%         If 'output_type" is 'polar' the real part represents the amplitude 
%         of the reflection coefficients and the imaginary part the phase
%         of the reflection coefficients.
% aux     structure with additional output. The following field is defined.
%     'critical_angle'   Critical angle in degrees
%
% EXAMPLE
%         vp=[1600;2000];
%         vs=[934;1634];
%         rho=[2.2;2.3];
%         angles=[0:5:50];
%         %     P-wave reflection coefficient for incoming P-wave
%         [reflect,aux]=zoeppritz_solid_solid(vp,vs,rho,angles,'P',[1 1 0 0],'complex');
%         figure
%         plot(angles,reflect)
%         legend('PP reflection','PS reflection')
%         xlabel('Angle in degrees')
%         grid on


emerging=logical(emerging);
if nargin < 7
   output_type='complex';
end

vp2=vp.^2;
vs2=vs.^2;

anglesDeg=anglesDeg(:)'*pi/180;

switch incident
case {'p','P'}
   p=sin(anglesDeg)/vp(1);
   p2=p.^2;
   cip=cos(anglesDeg)/vp(1);
   cis=sqrt(1/vs2(1)-p2);

case {'s','S'}
   p=sin(anglesDeg)/vs(1);
   p2=p.^2;   
   cis=cos(anglesDeg)/vs(1);
   cip=sqrt(1/vp2(1)-p2);

otherwise
   error(['Unrecognized type of incident wave: ',incident])
end

ctp=sqrt(1/vp2(2)-p2);
cts=sqrt(1/vs2(2)-p2);
temp1=rho(1)*(1.-(2.*vs2(1))*p2);
temp2=rho(2)*(1.-(2.*vs2(2))*p2);
dtemp=temp2-temp1;
temp1=temp1+2.*rho(2)*(vs2(2)*p2);
temp2=temp2+2.*rho(1)*(vs2(1)*p2);
drho=2.*(rho(2)*vs2(2)-rho(1)*vs2(1));
temp3=temp2.*cip+temp1.*ctp;
temp4=temp2.*cis+temp1.*cts;
temp5=dtemp-drho.*cip.*cts;
temp6=dtemp-drho.*ctp.*cis;
determ=temp3.*temp4+temp5.*temp6.*p2;
coeff=zeros(4,length(anglesDeg));

switch incident
case {'p','P'}
   if emerging(1)
      coeff(1,:)=((temp2.*cip-temp1.*ctp).*temp4-(dtemp+drho*cip.*cts).*temp6.*p2)./determ;
   end
   if emerging(2)
      coeff(2,:)=-(2*vp(1)/vs(1))*cip.*(dtemp.*temp2+drho*temp1.*ctp.*cts).*p./determ;
   end
   if emerging(3)
      coeff(3,:)=(2*vp(1)/vp(2))*rho(1)*cip.*temp4./determ;
   end
   if emerging(4)
      coeff(4,:)=(2*vp(1)/vs(2))*rho(1)*cip.*temp6.*p./determ;
   end

case {'s','S'}
   if emerging(1)
      coeff(1,:)=-(2*vs(1)/vp(1))*cis.*(dtemp.*temp2+drho*temp1.*ctp.*cts).*p./determ;
   end
   if emerging(2)
      coeff(2,:)=-((temp2.*cis-temp1.*cts).*temp3-(dtemp+drho*ctp.*cis).*temp5.*p2)./determ;
   end
   if emerging(3)
      coeff(3,:)=-(2*vs(1)/vp(2))*rho(1)*cis.*temp5.*p./determ;
   end
   if emerging(4)
      coeff(4,:)= (2*vs(1)/vs(2))*rho(1)*cis.*temp3./determ;
   end
end

coeff=coeff(emerging,:);

if strcmpi(output_type,'polar')
   amplitude=abs(coeff);
   if amplitude == 0
      phase=0;
   else
      phase=atan2(imag(coeff),real(coeff));
   end
   coeff=amplitude+i*phase;
end

%       Compute the critical angle if requested (i.e. if there is a second output argument)
if nargout > 1
   if vp(2) > vp(1)
      aux.critical_angle=asin(vp(1)/vp(2))*180/pi;
   else
      aux.critical_angle=90;
   end
end
