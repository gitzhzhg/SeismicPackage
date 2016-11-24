function imp=spike(v,n)
% SPIKE: create a signal with a single impulse in it
%
% imp=SPIKE(v,n)
% imp=SPIKE(v)
%
% returns a vector of the same size as v which is zero 
% except at sample n where it is 1.0 . n defaults to length(v)/2
%
% v= input signal vector used to determine the length
%    of the impulse vector. If a scalar, then the impulse
%    vector will be a column vector of length v.
% n= position of impulse vector ( default = length(v)/2 )
% imp = vector with a single nonzero unit impulse.
%
% by G.F. Margrave, May 1991
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

if length(v)==1, v=zeros(v,1); end
 if nargin==1
   n=round(length(v)/2);
 end

 if n>length(v)
   error(' n cannot be greater than length of vector')
 end
 imp=zeros(size(v));
 imp(n)=1.0;


 