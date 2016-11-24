function phase=unwrap_phase(phase)
% Unwrap phase as described in  Yanghua Wang, 2000, 
% A robust seismic phase unwrapping method;
% Journal of Seismic Exploration, 93-104
%
% Written by: E. Rietsch: March 4, 2004
% Last updated: July 12, 2005: simplification
%
%          phase=unwrap_phase(phase)   
% INPUT
% phase    wrapped phase (radians); a linear trend should have been removed.
% OUTPUT
% phase    unwrapped phase (radians)

[phase,ndims]=shiftdim(phase);  % Make sure that first dimension is not singleton

dphase=diff(phase);
bool=dphase > pi;
if any(bool)
   dphase(bool)=mod(dphase(bool)+pi,2*pi)-pi;
end

bool=dphase < pi;
if any(bool)
   dphase(bool)=-mod(-dphase(bool)-pi,2*pi)+pi;
end

dphase=[phase(1,:);dphase];

phase=shiftdim(cumsum(dphase),-ndims);  % Undo dimension change, if there was one
