function [x, options] = fmins(funfcn,x,options,grad,varargin)
%FMINS  Minimize function of several variables.
%   X = FMINS('F',X0) attempts to return a vector X which is a local
%   minimizer of F(x) near the starting vector X0.  'F' is a string
%   containing the name of the objective function to be minimized.
%   F(x) should be a scalar valued function of a vector variable.
%
%   X = FMINS('F',X0,OPTIONS) uses a vector of control parameters. If
%   OPTIONS(1) is positive, intermediate steps in the solution are
%   displayed; the default is OPTIONS(1) = 0.  OPTIONS(2) is the
%   termination tolerance for x; the default is 1.e-4.  OPTIONS(3) is
%   the termination tolerance for F(x); the default is 1.e-4.
%   OPTIONS(14) is the maximum number of function evaluations; the 
%   default is OPTIONS(14) = 200*length(x).  The other components of 
%   OPTIONS are not used as input control parameters by FMIN.  
%   For more information, see FOPTIONS.
%
%   X = FMINS('F',X0,OPTIONS,[],P1,P2,...) provides for additional
%   arguments which are passed to the objective function, F(X,P1,P2,...)
%   Pass an empty matrix for OPTIONS to use the default value.
%
%   [X,OPTIONS] = FMINS(...) returns the number of function evaluations
%   in OPTIONS(10).
%
%   FMINS uses a Nelder-Mead type simplex search method.
%
%   See also FMIN, FOPTIONS. 

%   Reference: J. E. Dennis, Jr. and D. J. Woods, New Computing
%   Environments: Microcomputers in Large-Scale Computing,
%   edited by A. Wouk, SIAM, 1987, pp. 116-122.

%   Copyright (c) 1984-96 by The MathWorks, Inc.
%   $Revision: 5.11 $  $Date: 1996/10/28 22:13:21 $

if nargin<3, options = []; end
options = foptions(options);
prnt = options(1);
tol = options(2);
tol2 = options(3);
% The input argument grad is there for compatability with FMINU in
% the Optimization Toolbox, but is not used by this function.

% Convert to inline function as needed.
funfcn = fcnchk(funfcn,length(varargin));

n = prod(size(x));
if (~options(14)) 
    options(14) = 200*n; 
end

% Set up a simplex near the initial guess.
xin = x(:); % Force xin to be a column vector
v = xin;    % Place input guess in the simplex! (credit L.Pfeffer at Stanford)
x(:) = v; fv = feval(funfcn,x,varargin{:}); 

% Following improvement suggested by L.Pfeffer at Stanford
usual_delta = 0.05;             % 5 percent deltas for non-zero terms
zero_term_delta = 0.00025;      % Even smaller delta for zero elements of x
for j = 1:n
   y = xin;
   if y(j) ~= 0
      y(j) = (1 + usual_delta)*y(j);
   else
      y(j) = zero_term_delta;
   end
   v = [v y];
   x(:) = y; f = feval(funfcn,x,varargin{:});
   fv = [fv  f];
end
[fv,j] = sort(fv);
v = v(:,j);


func_evals = n+1;
if prnt > 0
   clc
   format compact
   format short e
   home
   func_evals
   disp('initial ')
   disp(' ')
   v
   f
end

alpha = 1;  beta = 1/2;  gamma = 2;
[n,np1] = size(v);
onesn = ones(1,n); 
ot = 2:n+1;
on = 1:n;

% Iterate until the diameter of the simplex is less than tol.
while func_evals < options(14)
%   if max(max(abs(v(:,ot)-v(:,onesn)))) <= tol & ...
%          max(abs(fv(1)-fv(ot))) <= tol2
%       break

% -- My stuff
    if max(abs(fv(1:n+1))) <= tol2
        break
    end

    % One step of the Nelder-Mead simplex algorithm

    vbar = (sum(v(:,on)')/n)';
    vr = (1 + alpha)*vbar - alpha*v(:,n+1);
    x(:) = vr;
    fr = feval(funfcn,x,varargin{:}); 
    func_evals = func_evals + 1; 
    vk = vr;  fk = fr; how = 'reflect ';
    if fr < fv(n)
        if fr < fv(1)
            ve = gamma*vr + (1-gamma)*vbar;
            x(:) = ve;
            fe = feval(funfcn,x,varargin{:});
            func_evals = func_evals + 1;
            if fe < fv(1)
                vk = ve; fk = fe;
                how = 'expand  ';
            end
        end
    else
        vt = v(:,n+1); ft = fv(n+1);
        if fr < ft
            vt = vr; ft = fr;
        end
        vc = beta*vt + (1-beta)*vbar;
        x(:) = vc;
        fc = feval(funfcn,x,varargin{:}); 
        func_evals = func_evals + 1;
        if fc < fv(n)
            vk = vc; fk = fc;
            how = 'contract';
        else
            for j = 2:n
                v(:,j) = (v(:,1) + v(:,j))/2;
                x(:) = v(:,j);
                fv(j) = feval(funfcn,x,varargin{:}); 
            end
        func_evals = func_evals + n-1;
        vk = (v(:,1) + v(:,n+1))/2;
        x(:) = vk;
        fk = feval(funfcn,x,varargin{:}); 
        func_evals = func_evals + 1;
        how = 'shrink  ';
        end
    end
    v(:,n+1) = vk;
    fv(n+1) = fk;
    [fv,j] = sort(fv);
    v = v(:,j);

    if prnt > 0
        func_evals
        disp(how)
        disp(' ')
        v
        fv
    end
end
x(:) = v(:,1);
if prnt > 0, format, end
options(10)=func_evals;
options(8)=min(fv); 
if func_evals==options(14) 
    if options(1) >= 0
        disp(['Warning: Maximum number of function evaluations (', ...
               int2str(options(14)),') has been exceeded']);
        disp( '         (increase OPTIONS(14)).')
    end
end
