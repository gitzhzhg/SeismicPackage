%==============================================================================
% Anisotropic reflection tomography: Gauss-Newton optimization 
%==============================================================================

function F = artGNinv(param, Data, Ntrajectory, velCon)

global Ninterface NpolX NpolY IntPol 
global Xint Yint Zint Nint Rint
global Source Receiv Npair

clear global NReflector
global NReflector
NReflector(1:Ntrajectory+4) = 0;

weightVertVeloc = 0;
weightReflDepth = 0.5;

%==============================================================================

F(1) = 0;
inum = 1;

% Loop over ray trajectories
for trajectory=1:Ntrajectory
% Input ray code
  clear global RayCode Nsegment NMOCode 
  clear global p1 p2 q ugr vgr xray tau time
  global RayCode Nsegment NMOCode 
  global p1 p2 q ugr vgr xray tau time
  inverRayCode(trajectory);

% Input Cij's
  inverCij(param);

% Loop over source-receiver pairs
  for ipair=1:Npair

%   Compute Vnmo at three azimuths
    VnmoDat = compVnmo(Data(ipair,trajectory,4:6));

%   Shot the ray backward
    penalty = invershot(Data(ipair,trajectory,1), ...
                       [Data(ipair,trajectory,2), ...
                        Data(ipair,trajectory,3)], ipair);
    F(1) = F(1) + penalty;

%   Compute Vnmo
    if penalty < 1-eps;
      U = effCyl(p1, p2, q, xray, tau, NMOCode);
    else
      U = [1, 0; 0, 1];
    end;

%   Add Vnmo to the objective function
    VnmoCom = compVnmo([U(1,1), U(1,2), U(2,2)]);
    for j=1:3
      inum = inum + 1;
      if VnmoDat(j) > 0  &  VnmoCom(j) > 0
        F(inum) = abs(1 - VnmoCom(j)/VnmoDat(j));
      else
        F(inum) = 1;
        fprintf('>>> Imaginary NMO velocity encountered \n');
        pause
      end;
    end;

% End of the loop over source-receiver pairs
  end;

%F(1:inum)
%pause
%==============================================================================
% Reconstruct reflector taking into account different modes
  XintSave(:,trajectory) = Xint';   YintSave(:,trajectory,:) = Yint';
  ZintSave(:,trajectory) = Zint';   NintSave(:,:,trajectory) = Nint(:,:);

% Determine the number of reflected modes
  rtrajectory = trajectory+2;
  if NReflector(rtrajectory) == NReflector(rtrajectory+1);
    doitnow = 0;   mode = 0;
  elseif NReflector(rtrajectory) == NReflector(rtrajectory-1)  & ...
         NReflector(rtrajectory) ~= NReflector(rtrajectory-2);
    doitnow = 1;   mode = 2;
  elseif NReflector(rtrajectory) == NReflector(rtrajectory-1)  & ...
         NReflector(rtrajectory) == NReflector(rtrajectory-2);
    doitnow = 1;   mode = 3;
  else
    doitnow = 1;   mode = 1;
  end;
% [trajectory, doitnow, mode]
  
% Reconstruct reflector 
  if doitnow == 1;

%   One reflected mode
    if mode == 1;
      IntPol(:,:,Rint) = inverInt(Xint, Yint, Zint, Nint, NpolX, NpolY);

%   Two reflected modes 
    elseif mode == 2;
%     Compute two reflector positions
      IntPol0(:,:,Rint) = ...
        inverInt(XintSave(:,trajectory)', YintSave(:,trajectory)', ...
                 ZintSave(:,trajectory)', NintSave(:,:,trajectory), ...
                 NpolX, NpolY);
      IntPol1(:,:,Rint) = ...
        inverInt(XintSave(:,trajectory-1)', YintSave(:,trajectory-1)', ...
                 ZintSave(:,trajectory-1)', NintSave(:,:,trajectory-1), ...
                 NpolX, NpolY);
%     Add the stuff to the objective function
      for ipair=1:Npair
        Z0 = funInt(NpolX, NpolY, IntPol0, Rint, ...
                    Source(1,ipair), Source(2,ipair));
        Z1 = funInt(NpolX, NpolY, IntPol1, Rint, ...
                    Source(1,ipair), Source(2,ipair));
        inum = inum + 1;
        F(inum) = weightReflDepth*abs(1 - Z1/Z0);
%F(1:inum)
%trajectory
%pause
      end;       
%     Average the reflector
      XintAv = reshape(XintSave(:,trajectory-1:trajectory), 1, 2*Npair);
      YintAv = reshape(YintSave(:,trajectory-1:trajectory), 1, 2*Npair);
      ZintAv = reshape(ZintSave(:,trajectory-1:trajectory), 1, 2*Npair);
      NintAv = reshape(NintSave(:,:,trajectory-1:trajectory), 3, 2*Npair);
      IntPol(:,:,Rint) = inverInt(XintAv, YintAv, ZintAv, NintAv, ...
                                  NpolX, NpolY);
%%    IntPol

%   Three reflected modes
    else
%     Compute three reflector positions
      IntPol0(:,:,Rint) = ...
        inverInt(XintSave(:,trajectory)', YintSave(:,trajectory)', ...
                 ZintSave(:,trajectory)', NintSave(:,:,trajectory), ...
                 NpolX, NpolY);
      IntPol1(:,:,Rint) = ...
        inverInt(XintSave(:,trajectory-1)', YintSave(:,trajectory-1)', ...
                 ZintSave(:,trajectory-1)', NintSave(:,:,trajectory-1), ...
                 NpolX, NpolY);
      IntPol2(:,:,Rint) = ...
        inverInt(XintSave(:,trajectory-2)', YintSave(:,trajectory-2)', ...
                 ZintSave(:,trajectory-2)', NintSave(:,:,trajectory-2), ...
                 NpolX, NpolY);
%     Add the stuff to the objective function
      for ipair=1:Npair
        Z0 = funInt(NpolX, NpolY, IntPol0, Rint, ...
                    Source(1,ipair), Source(2,ipair));
        Z1 = funInt(NpolX, NpolY, IntPol1, Rint, ...
                    Source(1,ipair), Source(2,ipair));
        Z2 = funInt(NpolX, NpolY, IntPol2, Rint, ...
                    Source(1,ipair), Source(2,ipair));
        inum = inum + 1;
        F(inum) = (weightReflDepth/3)* ...
                  (abs(1 - Z1/Z0) + abs(1 - Z2/Z0) + abs(1 - Z2/Z1));
      end;
%     Average the reflector
      XintAv = reshape(XintSave(:,trajectory-2:trajectory), 1, 3*Npair);
      YintAv = reshape(YintSave(:,trajectory-2:trajectory), 1, 3*Npair);
      ZintAv = reshape(ZintSave(:,trajectory-2:trajectory), 1, 3*Npair);
      NintAv = reshape(NintSave(:,:,trajectory-2:trajectory), 3, 3*Npair);
      IntPol(:,:,Rint) = inverInt(XintAv, YintAv, ZintAv, NintAv, ...
                                  NpolX, NpolY);

    end;
%   Compute partial derivatives of the reflector
    pderInt;
% End of reconstructing of interfaces
  end;

% End of the loop over ray trajectories
end;

%------------------------------------------------------------------------------
% Equality constraints for the vertical velocities of P- and SV-waves
  velDiff = inverVelCon(param, Ntrajectory, velCon);
  F(inum+1:inum+length(velDiff)) = weightVertVeloc*abs(velDiff);

%==============================================================================
FF = sqrt(sum(F.^2));

  fprintf(['>>> %6.3f %6.3f %6.3f %6.3f %6.2f -- %9.2e \r'], [param, FF]);

%F
%pause
 
%==============================================================================

