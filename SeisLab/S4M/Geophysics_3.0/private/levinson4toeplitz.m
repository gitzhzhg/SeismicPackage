function x=levinson4toeplitz(ac,rhs)
% Solve the linear system of equations Tx = r using Levinson's recursion;
% T is the Toeplitz matrix
%        ( ac(1) ac(2) ac(3) ...                   )
%        ( ac(2) ac(1) ac(2) ac(3) ...             )
%     T= ( ac(3) ac(2) ac(1) ac(2) ac(3) ...       )
%        ( ac(4) ac(3) ac(2) ac(1) ac(2) ac(3) ... ) 
%        ( ....................................... )
% "Matrix Computations" by Golub and Van Loan, 
% Third Edition, Johns Hopkins University Press, 1996
% (p. 196 ff.)
% Written by: E. R.: July 29, 2004
% Last updated:
%
%        x=levinson4toeplitz(ac,rhs);
% INPUT
% ac     autocorrelation function
% rhs    right-hand side
% OUTPUT
% x      solution of Tx = rhs

ac=ac(:)/ac(1);
rhs=rhs(:);

a=ac(2:end);
nrhs=length(rhs);
x=zeros(size(rhs));
y=zeros(size(a));
z=zeros(size(a)); 
y(1)=-a(1);
x(1)=rhs(1);
beta=1;
alpha=-a(1);

for ii=1:nrhs-1
   beta=(1-alpha^2)*beta;
   mu=(rhs(ii+1)-a(1:ii)'*x(ii:-1:1))/beta;
   nu(1:ii)=x(1:ii)+mu*y(ii:-1:1);
   x(1:ii)=nu(1:ii);
   x(ii+1)=mu;
   if ii < (nrhs-1)
      alpha=-(a(ii+1)+a(1:ii)'*y(ii:-1:1))/beta;
      z(1:ii)=y(1:ii)+alpha*y(ii:-1:1);
      y(1:ii)=z(1:ii);
      y(ii+1)=alpha;
   end
end

 
