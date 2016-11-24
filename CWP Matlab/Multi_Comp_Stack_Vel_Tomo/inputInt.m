%==============================================================================
% Input of model interfaces
%==============================================================================
% Each interface is modeled as a 2-D polynomial. For convenience, the plane 
% parts of the interfaces are specified separately. 

% The plane portion of the interface is specified by:
%   depthPlane -- depth under the coordinate origin,
%   dipPlane   -- interface dip, and
%   azmPlane   -- azimuth of the dip plane.

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

function [] = inputInt;

global Ninterface NpolX NpolY IntPol 
global IntPolX IntPolY
global IntPolXX IntPolXY IntPolYY 

%depthPlane = [0.,   1.0,   2.0,   3.0];
%dipPlane   = [0.,  12.0,   5.0,  10.0];
%azmPlane   = [0.,  20.0, 170.0,  20.0];
depthPlane = [0.,   0.5];
dipPlane   = [0.,  30.0];
azmPlane   = [0.,   0.0];
Ninterface = length(depthPlane);    %%% number of interfaces

NpolX = 2;   %%% maximum dimension in x-direction 
NpolY = 2;   %%% maximum dimension in y-direction 
if NpolX < 2 | NpolY < 2
  fprintf('NpolX = %g,  NpolY = %g \n', NpolX, NpolY);
  error('Both NpolX and NpolY are supposed to be greater than 1');
end;

% Input the polynomial component
% !! The elements IntPol(1,1), IntPol(1,2), and IntPol(2,1)
% !! do not matter here -- see below
IntPol(:,:,1) = eps*ones([NpolY, NpolX]);
IntPol(:,:,2) = eps*ones([NpolY, NpolX]);

% Input the plane component
rad = pi/180;
for Iinterface=1:Ninterface
  IntPol(1,1,Iinterface) = eps + depthPlane(Iinterface);
  IntPol(1,2,Iinterface) = eps - ...
    tan(rad*dipPlane(Iinterface))*cos(rad*azmPlane(Iinterface));
  IntPol(2,1,Iinterface) = eps - ...
    tan(rad*dipPlane(Iinterface))*sin(rad*azmPlane(Iinterface));
end;

%IntPol
%pause

%==============================================================================
% Construct polynomial representations of the first- and second-order 
% derivatives of the model interfaces 
pderInt;

%==============================================================================

