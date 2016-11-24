function b = fzero(FunFcn,x,tol,trace,varargin)
%FZERO  Find zero of function of one variable. 
%   FZERO(F,X) tries to find a zero of F.  F is a string containing 
%   the name of a real-valued function of a single real variable.   
%   The value returned is near a point where F changes sign, or NaN 
%   if the search fails.  
%
%   FZERO(F,X), where X is a vector of length 2, assumes X is an 
%   interval where the sign of F(X(1)) differs from the sign of F(X(2)).
%   An error occurs if this is not true.  Calling FZERO with an interval  
%   guarantees FZERO will return a value near a point where F changes 
%   sign.
%
%   FZERO(F,X), where X is a scalar value, uses X as a starting guess. 
%   FZERO looks for an interval containing a sign change for F and 
%   containing X.  If no such interval is found, NaN is returned.  
%   In this case, the search terminates when the search interval 
%   is expanded until an Inf, NaN, or complex value is found. 
%
%   FZERO(F,X,TOL) sets the relative tolerance for the convergence test.  
%   FZERO(F,X,TOL,TRACE) displays information at each iteration when
%   TRACE is nonzero.
%   FZERO(F,X,TOL,TRACE,P1,P2,...) allows for additional arguments
%   which are passed to the function, F(X,P1,P2,...).  Pass an empty
%   matrix for TOL or TRACE to use the default value.
%
%   Examples
%       fzero('sin', 3) returns pi. Note the quotes around sin.  
%       Ordinarily, functions are defined in M-files.
%       fzero('abs(x)+1', 1) returns NaN since this function does
%       not change sign anywhere on the real axis (and does not have
%       a zero as well).
%       fzero('sin', 3, [], 1) returns pi, uses the default tolerance,
%       and displays iteration information.
%
%   See also ROOTS.

%   Copyright (c) 1984-98 by The MathWorks, Inc.
%   $Revision: 5.12 $  $Date: 1997/11/21 23:30:46 $

%  This algorithm was originated by T. Dekker.  An Algol 60 version,
%  with some improvements, is given by Richard Brent in "Algorithms for
%  Minimization Without Derivatives", Prentice-Hall, 1973.  A Fortran
%  version is in Forsythe, Malcolm and Moler, "Computer Methods
%  for Mathematical Computations", Prentice-Hall, 1976.

% Initialization

if nargin < 3 | isempty(tol), tol = eps; end
if nargin < 4 | isempty(trace), trace = 0; end
if trace 
  header = ' Func evals      x            f(x)          Procedure';
  step=' ';
  count = 0;
end
if  (~isfinite(x))
   error('Second argument must be finite.')
end

% Convert to inline function as needed.
FunFcn = fcnchk(FunFcn,length(varargin));

% Interval input
if (length(x) == 2) 
  a = x(1); 
  b = x(2);
  fa = feval(FunFcn,a,varargin{:});
  fb = feval(FunFcn,b,varargin{:});
  if any(~isfinite([fa fb])) | any(~isreal([fa fb]))
     error('Function values at interval endpoints must be finite and real.')
  end
  if trace
    disp(header)
    data = [a fa]; step='       initial';
    count = count + 1;
    disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
    data = [b fb]; step = '       initial';
    count = count + 1;    
    disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
  end
  if (fa > 0) == (fb > 0)
    error('The function values at the interval endpoints must differ in sign.')
  end

% Starting guess scalar input
elseif (length(x) == 1)
  fx = feval(FunFcn,x,varargin{:});
  if fx == 0
    b = x;
    return
  elseif ~isfinite(fx) | ~isreal(fx)
    error('Function value at starting guess must be finite and real.');
  end
  if trace 
     disp(header)
     data = [x fx]; step='       initial';
     count = count + 1;     
     disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
  end
  if x ~= 0, 
    dx = x/50;
  else, 
    dx = 1/50;
  end

  % Find change of sign.
  twosqrt = sqrt(2); 
  a = x; fa = fx; b = x; fb = fx;
  
  while (fa > 0) == (fb > 0)
     dx = twosqrt*dx;
     a = x - dx;  fa = feval(FunFcn,a,varargin{:});
     if ~isfinite(fa) | ~isreal(fa)
       disperr(a,fa);
       b = NaN;
       return
     end
     if trace 
        data = [a fa];  step='       search';
        count = count + 1;
        disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
     end
     if (fa > 0) ~= (fb > 0)
        break
     end
     b = x + dx;  fb = feval(FunFcn,b,varargin{:});
     if ~isfinite(fb) | ~isreal(fb)
       disperr(b,fb);
       b = NaN;
       return
     end
     if trace 
        data = [b fb];  step='       search';
        count = count + 1;        
        disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
     end
   end % while
   if trace
     disp(' ')
     disp(['   Looking for a zero in the interval [', ...
                 num2str(a) , ', ', num2str(b), ']']);
     disp(' ')
   end
else
   error('Second argument must be of length 1 or 2.');
end % if (length(x) == 2

fc = fb;
% Main loop, exit from middle of the loop
while fb ~= 0
   % Insure that b is the best result so far, a is the previous
   % value of b, and c is on the opposite of the zero from b.
   if (fb > 0) == (fc > 0)
      c = a;  fc = fa;
      d = b - a;  e = d;
   end
   if abs(fc) < abs(fb)
      a = b;    b = c;    c = a;
      fa = fb;  fb = fc;  fc = fa;
   end

   % Convergence test and possible exit
   m = 0.5*(c - b);
   toler = 2.0*tol*max(abs(b),1.0);
   if (abs(m) <= toler) + (fb == 0.0), break, end

   % Choose bisection or interpolation
   if (abs(e) < toler) + (abs(fa) <= abs(fb))
   % Bisection
      d = m;  e = m;
      step='       bisection';
   else
   % Interpolation
      s = fb/fa;
      if (a == c)
      % Linear interpolation
         p = 2.0*m*s;
         q = 1.0 - s;
      else
      % Inverse quadratic interpolation
         q = fa/fc;
         r = fb/fc;
         p = s*(2.0*m*q*(q - r) - (b - a)*(r - 1.0));
         q = (q - 1.0)*(r - 1.0)*(s - 1.0);
      end;
      if p > 0, q = -q; else p = -p; end;
      % Is interpolated point acceptable
      if (2.0*p < 3.0*m*q - abs(toler*q)) * (p < abs(0.5*e*q))
         e = d;  d = p/q;
         step='       interpolation';
      else
         d = m;  e = m;
         step='       bisection';
      end;
   end % Interpolation

   % Next point
   a = b;
   fa = fb;
   if abs(d) > toler, b = b + d;
   else if b > c, b = b - toler;
        else b = b + toler;
        end
   end
   fb = feval(FunFcn,b,varargin{:});
   if trace 
      data = [b fb];  
      count = count + 1;      
      disp([sprintf('%5.0f   %13.6g %13.6g ',count, data), step])
   end
end % Main loop


%------------------------------------------------------------------

function disperr(y, fy)
%DISPERR Display an appropriate error message when FY is Inf, 
%   NaN, or complex.  Assumes Y is the value and FY is the function 
%   value at Y. If FY is neither Inf, NaN, or complex, it generates 
%   an error message.

if ~isfinite(fy)  % NaN or Inf detected
       disp('NaN or Inf function value encountered during ');
       disp('   search for an interval containing a sign change.');
       disp(['Function value at ', num2str(y),' is ',num2str(fy)]);
       disp('Aborting since no such interval was found.')
       disp('Check function or try again with a different starting value.')
elseif ~isreal(fy) % Complex value detected
       disp('Complex function value encountered during ');
       disp('   search for an interval containing a sign change.');
       disp(['Function value at ', num2str(y),' is ',num2str(fy)]);
       disp('Aborting since no such interval was found.')
       disp('Check function or try again with a different starting value.')
else
       error('Disperr called with invalid argument.')
end
  
