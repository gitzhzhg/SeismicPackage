function y=DG_inv(id,DG,x)

% This function performs a non-linear
% inversion between DG_G, Drho_rho and
% Dbeta_beta parameters (used locally in the
% main routines *phase2.m).
%  
% id=1 get Dbeta/beta from DG_G and Drho/rho
% id=1 get Drho/rho from DG_G and Dbeta/beta

  if id==0
    y1=((2-0.5*x*DG)+sqrt((0.5*x*DG-2)^2-(DG-x)^2))/(0.5*(DG-x));
    y2=((2-0.5*x*DG)-sqrt((0.5*x*DG-2)^2-(DG-x)^2))/(0.5*(DG-x));
    if abs(y1)<abs(y2)
      y=y1;
    else
      y=y2;
    end;
  end;
  if id==1
    y=(2*x-DG-0.25*DG*x^2)/(0.5*x*DG-0.25*x^2-1);
  end;