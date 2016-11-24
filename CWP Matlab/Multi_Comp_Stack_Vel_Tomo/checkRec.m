%==============================================================================
% Check whether or not receivers sit on model interface
%==============================================================================

function [] = checkRec;

global Ninterface NpolX NpolY IntPol 
global Source Nsource Receiv Nreceiv
global RayCode

%==============================================================================
aaa = size(RayCode);  Iinterface = aaa(2);

for ireceiv=1:Nreceiv
  interf3 = funInt(NpolX, NpolY, IntPol, RayCode(3,Iinterface), ...
                   Receiv(1,ireceiv), Receiv(2,ireceiv));
  if abs(Receiv(3,ireceiv) - interf3) > 1.e+3*eps;
     fprintf('Receiver  %g has the depth = %g \n', ...
              ireceiv, Receiv(3,ireceiv));
     fprintf('Interface %g has the depth = %g \n', ...
              RayCode(3,Iinterface), interf3);
     error('Correct the receiver or interface position'); 
  end;
end; 

%==============================================================================

