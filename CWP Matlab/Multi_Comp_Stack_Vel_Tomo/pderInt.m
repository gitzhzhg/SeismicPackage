%==============================================================================
% Compute polynomial representations of the first- and second-order 
% derivatives of the model interfaces 
%==============================================================================

function [] = pderInt 

global Ninterface NpolX NpolY IntPol 
global IntPolX IntPolY
global IntPolXX IntPolXY IntPolYY 

%==============================================================================
% Matrices of the first partial derivatives
for Iinterface=1:Ninterface
  PolX = eps*ones([NpolY, NpolX-1]);
  flipPol = fliplr(IntPol(:,:,Iinterface));
  for iy=1:NpolY
    PolX(iy,:) = polyder(flipPol(iy,:));
  end;
  IntPolX(:,:,Iinterface) = fliplr(PolX);

  PolY = eps*ones([NpolY-1, NpolX]);
  flipPol = flipud(IntPol(:,:,Iinterface));
  for ix=1:NpolX
    PolY(:,ix) = polyder(flipPol(:,ix))';
  end;
  IntPolY(:,:,Iinterface) = flipud(PolY);
end;

%==============================================================================
% Matrices of the second partial derivatives
for Iinterface=1:Ninterface
  PolXX = eps*ones([NpolY, NpolX-2]);
  flipPol = fliplr(IntPolX(:,:,Iinterface));
  for iy=1:NpolY
    PolXX(iy,:) = polyder(flipPol(iy,:));
  end;
  IntPolXX(:,:,Iinterface) = fliplr(PolXX);

  PolXY = eps*ones([NpolY-1, NpolX-1]);
  flipPol = flipud(IntPolX(:,:,Iinterface));
  for ix=1:NpolX-1
    PolXY(:,ix) = polyder(flipPol(:,ix))';
  end;
  IntPolXY(:,:,Iinterface) = flipud(PolXY);

  PolYY = eps*ones([NpolY-2, NpolX]);
  flipPol = flipud(IntPolY(:,:,Iinterface));
  for ix=1:NpolX
    PolYY(:,ix) = polyder(flipPol(:,ix))';
  end;
  IntPolYY(:,:,Iinterface) = flipud(PolYY);
end;

%==============================================================================

