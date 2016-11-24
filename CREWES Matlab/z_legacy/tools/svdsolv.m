function [a,var]= svdsolv(u,s,v,data,vardata)
% solve a linear system given its sigular value decomposition
%
% [a,var] = svdsolv( u, s, v, data, vardata)
% [a,var] = svdsolv( u, s, v, data)
%
% u = u matrix from singular value decomposition (m by n with m > n)
% s = s matrix (diagonal containing the singular values) from svd (n by n)
% v = v matrix from svd (n by n)
% data = column vector of data forming the left hand side of the linear system
%          ( m by 1)
% vardata = estimated variance of the data. (Required to estimate the variance
%            of the model parameters.) If not specified, then it will be 
%            estimated from the scatter of the data about the final model.
% a = model parameters. (n by 1) These are the solution to the linear system)
% vara = estimated variance in a (n by 1). The standard errors in a would be
%        obtained by: stderr = vara.^(.5);
%
%  note: u,s, and v are most easily obtained from the Matlab command svd. If
%    X is the rectangular right hand matrix for the system x*a = data, where
%    X is m by n, a is n by 1, data is m by 1, and m > n, then u,s,v can be
%    obtained by: [u,s,v]=svd(x,0);  svdsolv checks for the presence of zero
%    singular values and does not invert them.
%
% by G.F. Margrave, May 1993
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
% test input
[m, n] = size(u); % m should be the length of data & n is the number of unknowns
[m2,n2]= size(data); % should be a column vector the size of u
% test for right sizing
 if( m2 ~= m )
    if( n2 ~= m)
	'data and svd matricies not of compatible sizes'
	a=[];
	var=[];
	return;
    end
    ' warning: data vector assumed to be transposed'
     data = data';
 end	
% generate the inverse matrix
singvals = diag(s);
ind = find( singvals ~= 0.0 );
singvals(ind) = 1. ./ singvals(ind); % invert the nonzero singular values
sinv = diag(singvals); % make a diagonal matrix
minv = v*sinv*u'; % the inverse matrix
 % compute the answer
 a = minv*data;
 % compute the variances
% test for default on vardata
if( nargin < 5)
	% compute the modeled data
	datamod = u*s*v'*a;
	% data variance is obtained from the sum squared error
	vardata = sum( (data - datamod).^2)/(m-n-1);
end
 var = zeros(size(a));
 for k = 1:n
      var(k) = sum((v(k,:).*singvals.').^2);
 end
 var = var.*vardata;
  
 