function [val] = fpequal(v1,v2,factor)
%FPEQUAL compares two floating point numbers and returns a logical value if
%they are equal.  In the case where vectors are being compared a vector the
%same size as the input vectors will be retuned  
%
% val=fpequal(v1,v2,factor);
%
% Floating point numbers can often be different slightly eventhough they
% appear the same.  Therfore using this function will give a better
% approximation to if they are equal or not.
%
% Inputs:
%       v1 - first number or vector to be compared
%       v2 - second number or vector to be compared
%       factor - the sensitivity, larger numbers equal less sensitivity
%               ************Default 100***********
%
% If Using vectors they must be the same size as each other
%
%
%G.F. Margrave & H.J.E Lloyd 2009
%
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

if (nargin<3), factor=100; end;

small=factor*eps;
val=abs(v1-v2)<small;

end