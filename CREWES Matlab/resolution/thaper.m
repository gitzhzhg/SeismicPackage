function theta = thaper(a,vo,c,z)
% THAPER: compute aperture-limited scattering angle versus depth
%
% theta = thaper(a,vo,c,z)
%
% Compute the zero offset scattering angle limit imposed
% by recording aperture.
%
% a ... aperture
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

if( c ~= 0)
	gamma = vo./(vo+c*z);
	
	top = ( 2*a*c*vo*gamma).^2;
	term=(a*a*c*c +vo*vo)^2;
	term2=(a*a*c*c - vo*vo);
	bot = term*gamma.^4 + 2*term2*(vo*gamma).^2 + vo^4;
	
	snth = sqrt(top./bot);
	theta=180*asin(snth)/pi;
	
	%test for theta or 180-theta
	csth = sqrt(1-snth.^2);
	atest = vo*(sqrt(1-(gamma.*snth).^2)-csth)./(c*gamma.*snth);
	ind = find( abs(atest-a)/a > 10^(-6));
	theta (ind) =180- theta(ind);
else
	ind= z==0;
	theta=zeros(size(ind));
	theta(~ind) = 180*atan(a./z(~ind))/pi;	
	theta(ind) = 90*ones(sum(ind),1);
	
end