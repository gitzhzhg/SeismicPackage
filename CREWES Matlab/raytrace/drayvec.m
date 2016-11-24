function drdt=drayvec(t,r)
% DRAYVEC: compute the derivative of ray vector (for vxz raytracing)
%
% drdt=drayvec(t,r)
%
% This is the fundamental driver for v(x,z) raytracing.
% The velocity model is defined by first calling rayvelmod.
% t ... scalar giveg the current time
% r ... 4x1 column vector giving (x,z,p,q)
% drdt ... 4x1 column vector giving the time-derivative of r
%
% By default, DRAYVEC uses nearest neighbor interpolation. Bilinear 
% interpolation is available for more accuracy. To get bilinear 
% interpolation, define a global variable called BILINEAR (all caps) 
% and set its value to 1. This is quite a bit slower than nearest 
% neighbor for the same grid size.
%
%
% by G.F. Margrave, CREWES, June 2000
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

global RTV2 RTDLNVDX RTDLNVDZ RTDG BILINEAR RTX RTZ 
if(isempty(RTV2))
   error('velocity model not defined. Use RAYVELMOD')
end
if(isempty(BILINEAR))
   BILINEAR=0;
end

[m,n]=size(RTV2);

x=r(1);z=r(2);p=r(3);q=r(4);

drdt=zeros(length(r),1);

%determine current position
xx=(x-RTX(1))/RTDG+1; zz=(z-RTZ(1))/RTDG+1;% Actual fractional grid
% guard against leaving model
xx=min([xx n-1]);xx=max([xx 1]);
zz=min([zz m-1]);zz=max([zz 1]); 

if(~BILINEAR)
   %nearest neighbor interpolation
   v2=RTV2(round(zz),round(xx));
   dlnvdx=RTDLNVDX(round(zz),round(xx));
   dlnvdz=RTDLNVDZ(round(zz),round(xx));
else


   %bilinear interpolation of v squared

   ix=floor(xx);%Upper left
   iz=floor(zz);%Upper left

   factx=xx-ix;
   factz=zz-iz;
   factxz=factx*factz;
   a=RTV2(iz,ix) + factx*(RTV2(iz,ix+1)-RTV2(iz,ix));
   b=RTV2(iz+1,ix) + factx*(RTV2(iz+1,ix+1)-RTV2(iz+1,ix));
   v2= a+factz*(b-a);

   %bilinear interpolation of logderivs
   a=RTDLNVDX(iz,ix) + factx*(RTDLNVDX(iz,ix+1)-RTDLNVDX(iz,ix));
   b=RTDLNVDX(iz+1,ix) + factx*(RTDLNVDX(iz+1,ix+1)-RTDLNVDX(iz+1,ix));
   dlnvdx=a+factz*(b-a);
   a=RTDLNVDZ(iz,ix) + factx*(RTDLNVDZ(iz,ix+1)-RTDLNVDZ(iz,ix));
   b=RTDLNVDZ(iz+1,ix) + factx*(RTDLNVDZ(iz+1,ix+1)-RTDLNVDZ(iz+1,ix));
   dlnvdz=a+factz*(b-a);

end

drdt(1) = v2*p;
drdt(2) = v2*q;
drdt(3) = -dlnvdx;
drdt(4) = -dlnvdz;