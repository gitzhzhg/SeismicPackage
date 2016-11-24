%==============================================================================
% Compute the depth x3 of the Iinterface-th interface at [x1, x2]
%==============================================================================

% Each interface is specified by:
%   NpolX  -- dimension in the x-direction; 
%   NpolY  -- dimension in the y-direction; 
%   IntPol -- [NpolY, NpolX]-dimensional matrix specifying the interface.
% !! Here and below, X = x1 and Y = x2.

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

%==============================================================================

function x3 = funInt(NpolX, NpolY, IntPol, Iinterface, x1, x2)

% Compute the value of 2-D polynomial x3(x1,x2)
flipInt = fliplr(IntPol(:,:,Iinterface));
for iy=1:NpolY
  y(iy) = polyval(flipInt(iy,:), x1);
end;
flipY = fliplr(y);
x3 = polyval(flipY, x2);

%==============================================================================

