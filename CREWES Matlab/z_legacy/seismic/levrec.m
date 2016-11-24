function x=levrec(aa,b);
% x=levrec(aa,b) 
%
% This function solves the matrix equation Tx=b for the vector
% x using Levinson recursion. The symmetric Toeplitz matrix T is
% assumed specified completely by the autocorrelation vector
% aa.
%        ( aa(1) aa(2) aa(3) aa(4) ...
%        ( aa(2) aa(1) aa(2) aa(3) aa(4) ... 
%     T= ( aa(3) aa(2) aa(1) aa(2) aa(3) aa(4) ...
%        ( aa(4) aa(3) aa(2) aa(1) aa(2) aa(3) aa(4) ...  
%        ( .............................................
% aa= input autocorrelation vector. If not normalized to aa(1)=1.0
%      then it will be
% b= input rhs vector (b will be converted to a column vector if
%     it is not already) 
% x= solution vector (column vector)
%
% The algorithm is taken from "Matrix Computations" by Golub and
% Van Loan, Second Edition, Johns Hopkins University Press, 1989
% (see page 187)
%
% Implemented by G.F. Margrave, May 1991  
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
% convert aa and b to column vectors if needed
 [l,m]=size(b);
  if l<m
    b=b';
  end
 [l,m]=size(aa);
  if l<m
    aa=aa';
  end
% normalize aa
 if aa(1)~= 1.0
   aa=aa/max(aa);
 end
% check for valid autocorr
 if aa(1)~= max(aa)
   error(' Invalid autocorrelation: zero lag not maximum')
 end
% initialize some stuff
 a=aa(2:length(aa));
 n=length(b);
 y=zeros(size(a));
 x=zeros(size(b));
 z=zeros(size(a)); 
 y(1)=-a(1);
 x(1)=b(1);
 beta=1;
 alpha=-a(1);
% main recursion loop
 for k=1:n-1
	beta=(1-alpha^2)*beta;
	mu=(b(k+1)-a(1:k)'*x(k:-1:1))/beta;
	nu(1:k)=x(1:k)+mu*y(k:-1:1);
	x(1:k)=nu(1:k);
   	x(k+1)=mu;
   	if k<(n-1)
     	alpha=-(a(k+1)+a(1:k)'*y(k:-1:1))/beta;
     	z(1:k)=y(1:k)+alpha*y(k:-1:1);
     	y(1:k)=z(1:k);
     	y(k+1)=alpha;
    end
 end
 