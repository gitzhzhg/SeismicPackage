%==============================================================================
% Define constants needed for ART
%==============================================================================

function [] = constants;  

global unity33 indexTM

%==============================================================================

% 3*3 unity matrix
  unity33 = diag(diag(ones(3)));

% Indeces for relating the 6*6 elastic stiffness matrix Cmatrix
% and the 3*3*3*3 tensor Ctensor. The relaton is given by
% Ctensor(i1,i2,j1,j2) = Cmatrix(indexTM(i1,i2),indexTM(j1,j2))
  indexTM = [1, 6, 5; 6, 2, 4; 5, 4, 3];

%==============================================================================


