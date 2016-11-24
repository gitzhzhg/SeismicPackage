function bin = binom(a,n,signum)
% Function computes n binomial coefficients
%      1 sign*a (sign^2)*a*(a-1)/2 (sign^3)*a*(a-1)*(a-2)/3! ...
% If a is a positive integer n can be dropped (it is then set to a+1)
%	bin = binom(a,n)
%
% See also: binomial_coefficients

if nargin < 3
   signum=1;
end

if nargin < 2
   n=a+1;
end

if n < 1 
   fprintf(' ERROR in binom; n < 1 \n')
   return
end
bin=ones(n,1);
ai=a;
for i=1:n-1
   bin(i+1)=signum*bin(i)*ai/i;
   ai=ai-1;
end


