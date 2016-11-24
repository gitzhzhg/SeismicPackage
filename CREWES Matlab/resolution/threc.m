function [theta,z] = threc(t,vo,c,z)
% THREC: Compute record-length-limit to scattering angle for v(z)
%
% theta = threc(t,vo,c,z)
% [theta,z]= threc(t,vo,c,nz)
%
% Compute the zero offset scattering angle limit imposed
% by recording time.
%
% t ... maximum recording time
% vo ... initial velocity
% c ... accelerator ( v(z) = vo +c*z )
% z ... vector of depths for which theta is computed
% theta ... vector of limiting scattering angles. One for each z.
%
% G.F. Margrave, CREWES, 1997
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

%test for automatic z calculation
if (length(z) ==1)
	nz=z;
	if( c~=0)
		zmax= vo*(exp(c*t/2)-1)/c;
	else
		zmax= vo*t/2;
	end
	z=linspace(0,zmax-100000*eps,nz);
end

if( c ~= 0)
	gamma = vo./(vo+c*z);
	arg=c*t/2;
	
	csth = (1 ./gamma -cosh(arg) )/sinh(arg);
	theta=180*acos(csth)/pi;
	
else
	zmax=vo*t/2;
	theta=180*acos(z/zmax)/pi;
	
end