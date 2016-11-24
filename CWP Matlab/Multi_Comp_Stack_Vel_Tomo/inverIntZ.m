%==============================================================================
% Find polynomial representation of model interface 
%==============================================================================

function IntPol = inverIntZ(Xint, Yint, Zint, NpolX, NpolY);

% Input:
% Xint, Yint, Zint -- arrays of points which are supposed to belong 
%                     to the interface;
% Nint             -- vector of interface normals at [Xint, Yint, Zint];
% NpolX, NpolY     -- dimensions of the polynomial describing the interface
%                     in x- and y-directions

% Output:
% IntPol -- polynomial describing the interface

%             The matrix IntPol has the following structure:

%                      x^0      |    x^1      |    x^2      ...
%                 +-------------+-------------+----------------
%             y^0 | IntPol(1,1) | IntPol(1,2) | IntPol(1,3) ...
%                 +-------------+-------------+----------------
%             y^1 | IntPol(2,1) | IntPol(2,2) | IntPol(2,3) ...
%                 +-------------+-------------+----------------
%             y^2 | IntPol(3,1) | IntPol(3,2) | IntPol(3,3) ...
%                 +-------------+-------------+----------------
%             ... |    ...      |     ...     |     ...        

% Technique: 
% Solving normal equation for unknown matrix IntPol. For inversion, 
% the unknown elements of IntPol are arranged the following way:
%   IntPol(1,1),     IntPol(2,1),     ..., IntPol(NpolY,1),
%   IntPol(1,2),     IntPol(2,2),     ..., IntPol(NpolY,2), ...
%   IntPol(1,NpolX), IntPol(2,NpolX), ..., IntPol(NpolY,NpolX). 

%==============================================================================
Ndata = length(Xint);
Munkn = NpolX*NpolY;
if Ndata < Munkn;
  fprintf('*** inverInt *** \n');
  error('The data are insufficient to reconstruct the interface \n');
end;

% Build matrix elements that describe the interface depths.
% Ideally, the depths satisfy the scalar equation:
%   Yint(n)^(iy-1)*IntPol(iy,ix)*Xint(n)^(ix-1) = Zint(n)
for n=1:Ndata
  for ix=1:NpolX
    for iy=1:NpolY
      m = iy + (ix-1)*NpolY;
      A(n,m) = Yint(n)^(iy-1)*Xint(n)^(ix-1);
    end;
  end;
  B(n,1) = Zint(n);
end;

%==============================================================================
% Solve normal eqs
Pol = pinv(A'*A)*A'*B;
IntPol = reshape(Pol,NpolY,NpolX);

%==============================================================================

