%==============================================================================
% Compute the length of the ray segment that hits interface
%==============================================================================

function diff = hitInt(length, ugr, x0, isegment);

global Ninterface NpolX NpolY IntPol
global RayCode
global r

%==============================================================================
interface = RayCode(3,isegment);

% Construct the position of the point on the ray 
r = x0 + ugr*length;

% Compute the depth of the interface at [r(1), r(2)]
depth = funInt(NpolX, NpolY, IntPol, interface, r(1), r(2));
diff = depth - r(3);

%==============================================================================

