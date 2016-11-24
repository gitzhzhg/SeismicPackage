% AUTHOR: V. Grechka, Colorado School of Mines, Center for Wave
% Phenomena, slightly modified by P. Jilek (January 2002). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% P-wave reflection coefficients from a stack of horizontal anisotropic (up
% to MNC with the horizontal symmetry plane) layers as functions of
% incidence slowness vector. Here, I try to realize a somewhat simplified
% version of reflectivity method
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INPUT:
% model.in ... input file of the medium parameters. See README file for
%              the syntax.
% some initial settings defining the ranges of computation, see below
%
% OUTPUT:
% model.out ... results of computation, containing reflection coefficients
%               and some other quantities, see README file for
%               details.
% angle.out ... a separate file with incidence angles and azimuths
%               evaluated; used fot testing purposes
% Singular.out ... this file is generated only if S-wave singularity
%                  occurs and polarization vectors of S-wave are messed
%                  up. It provides the information about the point of the
%                  singularity occurance.
% screen output ... informs about the stage the computation is currently in
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;   close all;

% ==========================
% START OF INITIAL SETTINGS:
% ==========================

px_s=0        % starting value of the x- slowness component px of the incident
              %  wave  
Dpx=0.01      % increment in px
n_px=10       % number of px values (>=1)

py_s=0.05        % starting value of the y- slowness component px of the incident
              %  wave  
Dpy=0.05      % increment in py
n_py=3        % number of py values (>=1)

% Open output file - here you can change the names of output files:
  f1out = fopen('model.out', 'w');   
  f2out = fopen('angle.out','w');

% ==========================
% END OF INITIAL SETTINGS:
% ==========================

%====================
% COMPUTATION BEGINS:
%====================

fprintf('\n');
%fprintf(['    p1       p2     angle     azim', ...
%         '        Rpp            Rps1            Rps2          S1gx          S1gy', ...
%         '        S1gz           S2gx             S2gy          S2gz \n']);
fprintf(['    p1       p2     angle     azim  \n']);

% Loop over frequences of incident wave
% -------------------------------------
  for iHz=1:1
%  Wavelet frequency (Hz)
%% f = 2*(iHz-1);   
   f = iHz-1;   
   w = 2*pi*f;

% Loop over slowness of incident wave
% -----------------------------------
  for ip1=0:n_px-1
    p1 = px_s + Dpx*ip1;
    for ip2=0:n_py-1
    p2=tan(67*pi/180)*p1;
     p2 = py_s + Dpy*ip2;

% Open file containing the model 
  fin = fopen('model.in', 'r');

% Read the number of layers between two halfspaces
Nlayer = fscanf(fin, '%d', 1);

% The main loop over the model
% ----------------------------

% Read the parameters for the upper half-space:

ro1 = fscanf(fin, '%g', 1);
cij = fscanf(fin, '%g', 12);   C  = FormCij(cij);
azm = fscanf(fin, '%g', 1);    C1 = RotateCij(C, azm);
p3 = ChristEq(C1, p1, p2);

[B1, B1inv] = InterMatrix(w, ro1, C1, p1, p2, p3);
B=Polar(C1, p1, p2, p3);
Slns_3=p3;

% Calculate wavelength and some other stuff for incident P wave:

if f > eps
   wavelength = 1/( f*sqrt(p1^2 + p2^2 + p3(1)^2) );
else
   wavelength = 99.999;
end;
incangle   = atan2( (sqrt(p1^2 + p2^2)), p3(1) )*180/pi;
if incangle < 1.e-4
   incazim = 0;
else
   incazim = atan2(p2, p1)*180/pi;
end;

% Read the parameters for the next layer:

ro2 = fscanf(fin, '%g', 1);
cij = fscanf(fin, '%g', 12);   C  = FormCij(cij);
azm = fscanf(fin, '%g', 1);    C2 = RotateCij(C, azm);
p3 = ChristEq(C2, p1, p2);

% Take into account the first interface

[B2, B2inv] = InterMatrix(w, ro2, C2, p1, p2, p3);
MM = B2inv*B1;


if Nlayer ~= -1
%..Loop over plane layers
   tau = 0;   Vnmosum2 = 0;
   for ilayer=1:Nlayer
      z = fscanf(fin, '%g', 1);
      L = LayerMatrix(w, z, p3);
      B1 = B2;   B1inv = B2inv;   ro1 = ro2;   C1 = C2;   
      ro2 = fscanf(fin, '%g', 1);
      cij = fscanf(fin, '%g', 12);   C  = FormCij(cij);
      azm = fscanf(fin, '%g', 1);    C2 = RotateCij(C, azm);

      p3 = ChristEq(C2, p1, p2);
      [B2, B2inv] = InterMatrix(w, ro2, C2, p1, p2, p3);
      MM = B2inv*B1*L*MM;
   end;
end;
status = fclose(fin);

% Compute the generalized P-wave reflection coefficient

R = ReflPP(MM);   

% ================
% VARIOUS OUTPUTS:
% ================
%
%fmem(iHz) = f;   Rmem(iHz) = abs(R);   

%fprintf(['%7.2f %7.3f %8.4f %8.4f %7.2f %9.2f %14.5e %14.5e \n'], ...
%        f, wavelength, p1, p2, incangle, incazim, real(R), imag(R));
%fprintf(fout, ['%7.2f %7.3f %8.4f %8.4f %7.2f %9.2f %14.5e %14.5e \n'], ...
%        f, wavelength, p1, p2, incangle, incazim, real(R), imag(R)); 
%
%fprintf(['%8.4f %8.4f %7.2f %9.2f %14.5e %14.5e %14.5e %14.5e %14.5e %14.5e 14.5e %14.5e %14.5e %14.5e %14.5e %14.5e \n'],...
%        p1, p2, incangle, incazim, R, B(:,2), B(:,3), Slns_3);

fprintf(['%8.4f %8.4f %7.2f %9.2f \n'], p1, p2, incangle, incazim);
fprintf(f1out, ['%8.4f %8.4f %7.2f %9.2f %14.5e %14.5e %14.5e %14.5e %14.5e %14.5e %14.5E %14.5e %14.5e %14.5e %14.5e %14.5e \n'],...
        p1, p2, incangle, incazim, R, B(:,2), B(:,3), Slns_3);
fprintf(f2out, ['%7.2f %9.2f \n'], incangle, incazim );

%Vel(1)=1/sqrt(p1^2+p2^2+Slns_3(1)^2);
%Vel(2)=1/sqrt(p1^2+p2^2+Slns_3(2)^2);
%Vel(3)=1/sqrt(p1^2+p2^2+Slns_3(3)^2);

%fprintf(f1out, ['%7.2f %7.2f %7.2f %7.4f %7.4f %7.4f %7.3f \n'], incangle, p1, p2, Vel, R(1));


%fprintf(['%8.4f %8.4f %7.2f %9.2f %14.5e %14.5e %14.5e %14.5e %14.5e %14.5e %14.5E %14.5e %14.5e \n'],...
%        p1, p2, incangle, incazim, R);
%plot(incangle, R(1), 'm*');
%plot(incangle, R(2), 'bs');
%plot(incangle, R(3), 'gd');

% End the loop over slowness
    
    end;
  end; 

% End the loop over frequences
  
end;

%========= END OF STORY ============






