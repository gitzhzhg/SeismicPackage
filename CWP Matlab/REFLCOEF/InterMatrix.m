%=============================================================================
% Construct interface matrix 
%=============================================================================

function [B, Binv] = InterMatrix(w, ro, c, p1, p2, p3)

%=============================================================================

% Calculate polarization vectors
A = Polar(c, p1, p2, p3);
%p1
% p2
% p3
% A
%=============================================================================

% Interface matrix consists of stress and displacement submatrices:
% B = [Sdw, Sup; Ddw, Dup]

N = [1,6,5; 6,2,4; 5,4,3];   
% UpDw = [1,1,-1; 1,1,-1; -1,-1,1];
p(1) = p1;   p(2) = p2;

% Construct matrices Sdw and Sup
for iq=1:3
   p(3) = p3(iq);
   for j=1:3
      Sdw(j,iq) = 0;   Sup(j,iq) = 0;
      for k=1:3
         for l=1:3
            term = c(N(3,j),N(k,l))*p(k)*A(l,iq);
            Sdw(j,iq) = Sdw(j,iq) + term; 
%           Sup(j,iq) = Sup(j,iq) + term*UpDw(k,l); 
         end;
      end;
   end;
end;
% ?? -- May be, due to structure of eqs, it is possible to omit factor i*w in
% Sdw = i*w*ro*Sdw;   
Sdw = ro*Sdw;
% Sup = i*w*ro*Sup;

% Instead of computing Sup, we can find it using symmetry: -- Check it!!
Sup(1:2,:) = -Sdw(1:2,:);   Sup(3,:) = Sdw(3,:);

%=============================================================================

% Construct matrices Ddw and Dup
Ddw = A;
Dup(1:2,:) = Ddw(1:2,:);   Dup(3,:) = -Ddw(3,:);

% ... and get matrix B:
B = [Sdw, Sup; Ddw, Dup];

%=============================================================================

% Invert matrix B using Frobenius formula

c_number=rcond(Dup);
if c_number < 0.0000000001
  fprintf('*** InterMatrix: S-wave singularity occurance in halfspace %i; messed up polarizations *** \n',id);
  err_flag=1;
  fid00=fopen('Singular.out','a');
  pomalost=[p1 p2 p3(1) p3(2) p3(3)];
  polar_P=A(:,1)';
  polar_S1=A(:,2)';
  polar_S2=A(:,3)';
  fprintf(fid00,'*** InterMatrix: S-wave singularity occurance in halfspace %i; messed up polarizations *** \n',id);
  fprintf(fid00,' slownesses: \n');
  fprintf(fid00,'p1_R=%f p2_R=%f p3P_R=%f p3S1_R=%f p3S2_R=%f \n p1_I=%f p2_I=%f p3P_I=%f p3S1_I=%f p3S2_I=%f \n',real(pomalost),imag(pomalost));
  fprintf(fid00,'\n polarizations: \n');
  fprintf(fid00,'Px_R=%f Py_R=%f Pz_R=%f \n Px_I=%f Py_I=%f Pz_I=%f \n',real(polar_P),imag(polar_P));
  fprintf(fid00,'S1x_R=%f S1y_R=%f S1z_R=%f \n S1x_I=%f S1y_I=%f S1z_I=%f \n',real(polar_S1),imag(polar_S1));
  fprintf(fid00,'S2x_R=%f S2y_R=%f S2z_R=%f \n S2x_I=%f S2y_I=%f S2z_I=%f \n',real(polar_S2),imag(polar_S2));
  fprintf(fid00,'\n medium: \n');
  fprintf(fid00,'c11_R=%f c12_R=%f c13_R=%f c14_R=%f c15_R=%f c16_R=%f \n',real(c(1,:)));
  fprintf(fid00,'c21_R=%f c22_R=%f c23_R=%f c24_R=%f c25_R=%f c26_R=%f \n',real(c(2,:)));
  fprintf(fid00,'c31_R=%f c32_R=%f c33_R=%f c34_R=%f c35_R=%f c36_R=%f \n',real(c(3,:)));
  fprintf(fid00,'c41_R=%f c42_R=%f c43_R=%f c44_R=%f c45_R=%f c46_R=%f \n',real(c(4,:)));
  fprintf(fid00,'c51_R=%f c52_R=%f c53_R=%f c54_R=%f c55_R=%f c56_R=%f \n',real(c(5,:)));
  fprintf(fid00,'c61_R=%f c62_R=%f c63_R=%f c64_R=%f c65_R=%f c66_R=%f \n \n',real(c(6,:)));
  fprintf(fid00,'c11_I=%f c12_I=%f c13_I=%f c14_I=%f c15_I=%f c16_I=%f \n',imag(c(1,:)));
  fprintf(fid00,'c21_I=%f c22_I=%f c23_I=%f c24_I=%f c25_I=%f c26_I=%f \n',imag(c(2,:)));
  fprintf(fid00,'c31_I=%f c32_I=%f c33_I=%f c34_I=%f c35_I=%f c36_I=%f \n',imag(c(3,:)));
  fprintf(fid00,'c41_I=%f c42_I=%f c43_I=%f c44_I=%f c45_I=%f c46_I=%f \n',imag(c(4,:)));
  fprintf(fid00,'c51_I=%f c52_I=%f c53_I=%f c54_I=%f c55_I=%f c56_I=%f \n',imag(c(5,:)));
  fprintf(fid00,'c61_I=%f c62_I=%f c63_I=%f c64_I=%f c65_I=%f c66_I=%f \n',imag(c(6,:)));
  fclose(fid00);
  stop;
end;

Dinv = inv(Dup);
ak = Sdw - Sup*Dinv*Ddw;   Kinv = inv(ak);
Binv = [Kinv,             -Kinv*Sup*Dinv; ...
        -Dinv*Ddw*Kinv,   Dinv + Dinv*Ddw*Kinv*Sup*Dinv];

% B
% Binv
% B*Binv

%=============================================================================


