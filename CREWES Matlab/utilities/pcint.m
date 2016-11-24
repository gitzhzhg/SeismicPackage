function yi=pcint(x,y,xi)
% PCINT: piecewise constant interpolation
%
% yi=pcint(x,y,xi)
%
% pcint does piecewise constant interpolation.
% That is, x and y are assumed to represent a function that is
% "blocky" or piecewise constant. This means that for any xi
% between x(k) and x(k+1), the function evaluates to y(k).
% Points with xi < x(1) evaluate to y(1) and for xi>x(length(x))
% evaluate to y(length(x))
% NOTE xi must be sorted in ascending order. (xi(k)<xi(k+1))
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

yi=nan*zeros(size(xi));
iiold=1;
nx=length(x);
%handle ends
j=find(xi<x(1));
if(~isempty(j))
	yi(j)=y(1)*ones(size(j));
end
j=find(xi>x(length(x)));
if(~isempty(j))
	yi(j)=y(length(x))*ones(size(j));
end
j=find(isnan(yi));
j=j(:)';
for k=j

		ii=find(x<=xi(k));

		yi(k)= y(ii(length(ii)));

end