%==============================================================================
% Compute the unit vector normal to the Iinterface-th interface at [x1, x2]
%==============================================================================

function normal = normInt(Iinterface, x1, x2)

%==============================================================================

global Ninterface NpolX NpolY IntPol
global IntPolX IntPolY

% Compute the first-order partial derivatives of the interface
n1 = funInt(NpolX-1, NpolY,   IntPolX, Iinterface, x1, x2);
n2 = funInt(NpolX,   NpolY-1, IntPolY, Iinterface, x1, x2);

%==============================================================================
% Construct the normal
nnorm = [-n1, -n2, 1];

% and normalize it
nrm = sqrt(sum(nnorm.^2));
normal = nnorm'/nrm;
%==============================================================================

