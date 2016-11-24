function rho=gardner(v,a,m,rho)
% rho=gardner(v,a,m,rho)
% rho=gardner(v,a,m)
%
% GARDNER estimates a density vector given an instantaneous velocity vector
% and values for the empirical parameters a and m via the Gardner relation
%	rho=a*v.^m (a times v to the mth power)
% If a partial density vector is available, then it may be provided as a 4th
% argument which must be expanded with NaN's to be the same length as v. (Take 
% care that the existing densities are paired with the proper v's.) Densities
% will only be computed with Garnders relation where they are missing and any
% non-NaN input densities are simply passed to output.
% 	v= vector of instantaneous p-wave velocities
%	a= scalar multplier
%		*********** default is .31 **********
%	m= scalar exponent
%		*********** default is .25 **********
%	rho= on input, a vector of known densities which must be expanded with 
%		NaN's to be the same size as v
%	rho= on output, aa vector of densities the same size as v and computed
%		via Gardner's relation where densities were missing on input.
%
% G.F. Margrave, March 1994
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
if(nargin<4)
	rho=nan*ones(size(v));
end
if(nargin<3)
	m=.25;
end
if(nargin<2)
	a=.31;
end
ind=find(isnan(rho));
rho(ind)= a*v(ind).^m;