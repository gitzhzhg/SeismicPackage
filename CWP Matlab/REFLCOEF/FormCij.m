%=============================================================================
% Construct the symmetric [6*6] Cij matrix from input array of Cij's   
%=============================================================================

function C = FormCij(cij)

% The order of Cij's on input is: 
% c11, c12, c13, c16, c22, c23, c26, c33, c36, c44, c55, c66

C = zeros(6);
C(1,1) = cij(1);   C(1,2) = cij(2);   C(1,3) = cij(3);   C(1,6) = cij(4);
                   C(2,2) = cij(5);   C(2,3) = cij(6);   C(2,6) = cij(7);
                                      C(3,3) = cij(8);   C(3,6) = cij(9);
C(4,4) = cij(10);  C(5,5) = cij(11);  C(6,6) = cij(12);

% Add some small deviation from isotropy or VTI, if necessary
if abs(C(4,4) - C(5,5)) < 1.e-4
   C(4,4) = C(4,4)*(1 - 1.e-4);
   C(5,5) = C(5,5)*(1 + 1.e-4);
end;

% Make symmetric matrix
for i=1:6
   for j=1:i-1
      C(i,j) = C(j,i);
   end;
end;
      
%=============================================================================
% Check if the condition of positive energy is satisfied
for i=1:6
   dd = det(C(1:i,1:i));
   if dd < 0
      fprintf('*** Condition of positive energy is not satisfied \n');
      fprintf('*** Medium is not physical \n \n');
      fprintf('*** Execution is terminated \n \n');
      stop
   end;
end;
%=============================================================================

