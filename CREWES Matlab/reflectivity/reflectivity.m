function [uR2,uZ2,t] = reflectivity (model,parameters,geometry)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    REFLECTIVITY MODELING CODE
%
%     LUIZ LOURES,  CALGARY, 08 / 02 / 2003
%
%   INPUT:
%      ELASTIC MODEL: 
%                                   model -> one row per layer of the model in the following format:
%
%                                       thickness(km) density(g/cm^3) vp(km/s)  Qp  vs(km/s)  Qs                                          
%
%      PARAMETERS INVOLVED IN FREQUENCY-SLOWNESS (WAVENUMBER) (F-K) DOMAIN MODELING AND FOURIER TRANSFORMS:
%                                   parametres ->   
%                                       T_max:     maximum modeling time (sec);
%                                       delta_T:    time interval (msec);
%                                       u1:            initial slowness to compute (sec/km); 
%                                       u2:            final slowness to compute (sec/km);
%                                       f1:             initial frequency (Hz) to model;
%                                       f2:             final frequency (Hz) to model;
%                                       tau:           wrap-around attenuation factor 
%                                                          (integrated as the imaginary part of complex velocity);
%                                       z_Source: Depth source (km)
%                                       perc_U:     percentage of the slowness integral that is windowed 
%                                       perc_F:      percentage of the frequency integral that is windowed
%                                       F1:            source component;
%                                       F2:       
%                                       F3:
%                                       direct:      = 1  compute direct wave; = 0 do not compute;
%                                       mult:        = 1  compute multiples;     = 0 do not compute;
%                                       delta_U:   slowness integral step length
%                                      
%      GEOMETRY PARAMETERS INDICATING RECEIVER POSITIONS: 
%                                   geometry -> depths of receivers from free surface (ground)
%
%   OUTPUT: 
%                   vertical component -> uZ2 matrix
%                   radial   component -> uR2 matrix
%                   time series               -> t vector
%    REFERENCES
%          Muller, G: The Reflectivity Method: a Tutorial, Journal of Geophysics
%            (1985) 58: 153-174                                         
%                                                                       
%          Mallick S. and Fraser N.: Practical Aspects of Reflectivity Modeling  
%            Geophysics (1987) 52 no 10: 1355-1364
%
% NOTE: This SOFTWARE may be used by any individual or corporation for any purpose
% with the exception of re-selling or re-distributing the SOFTWARE.
% By using this software, you are agreeing to the terms detailed in this software's
% Matlab source file.

% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by
% its 'AUTHOR' (identified above) and the CREWES Project.  The CREWES
% project may be contacted via email at:  crewesinfo@crewes.org
%
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) This SOFTWARE may be used by any individual or corporation for any purpose
%    with the exception of re-selling or re-distributing the SOFTWARE.
%
% 2) The AUTHOR and CREWES must be acknowledged in any resulting publications or
%    presentations
%
% 3) This SOFTWARE is provided "as is" with no warranty of any kind
%    either expressed or implied. CREWES makes no warranties or representation
%    as to its accuracy, completeness, or fitness for any purpose. CREWES
%    is under no obligation to provide support of any kind for this SOFTWARE.
%
% 4) CREWES periodically adds, changes, improves or updates this SOFTWARE without
%    notice. New versions will be made available at www.crewes.org .
%
% 5) Use this SOFTWARE at your own risk.
%
% END TERMS OF USE LICENSE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modification comments:
% 1.  The "reflectivity.m" file has been chaged into a function from its original script version;
% 2.  Some formulation error in coding has been corrected. For details, see "RT.m". 
%      This may account for the incorrect arrival times for S waves before corrections;
% 3.  Codes were modified and corrected by Yongwang Ma in November 2004.

%input parameters
T_max = parameters(1);
delta_T = parameters(2);
u1 = parameters(3);
u2 = parameters(4);
f1 = parameters(5);
f2 = parameters(6);
tau = parameters(7);
z_Source = parameters(8);
perc_U = parameters(9)/100;
perc_F = parameters(10)/100;
F1 = parameters(11);
F2 = parameters(12);
F3 = parameters(13);
direct = parameters(14);
mult = parameters(15);
delta_U = parameters(16);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% elastic parameters
nL = length(model(:,1));     % how many layers

thick = model(:,1);          % thickeness
rho = model(:,2);            % density
alpha = model(:,3);          % p-wave velovity
Qp = model(:,4);             % Qp factor
beta = model(:,5);           % s-wave velocity
Qs = model(:,6);             % Qs  factor

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some important variabel

n_rec = length(geometry);                 % # of geophones
nSamples = round(T_max / delta_T) + 1;    % # of samples
T_max = delta_T * (nSamples - 1);         %  time to model
delta_F = 1 / T_max;                                % frequency intervall to model
nF = round((f2 - f1) / delta_F);               % # of frenquecy to model
nU = round((u2 - u1) / delta_U)+1;       % # of slowness to model                      
u2 = nU * delta_U;                                    % setting the maximun slowness u2 to model
f2 = f1 + nF * delta_F;                              % setting the maximun slowness u2 to model

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% attenuation of wrap-around  (for details refer Aki & Richards, Section 9.2  and 
%                                Mallick an Frazer (1987) )

tau = log(tau) / T_max;
if tau > 7.5
    tau = 7.5;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% source is at 1st layer

%thick(1) = thick(1)- z_Source;
zm = thick(1);   % set the reference level at the bottom of the first layer
alpham = alpha(1);
betam = beta(1);
rhom = rho(1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the hanning window for the slowness integral  (Mallick an Frazer  (1987),Figure 5b)
                                                   
window_slowness = hanning_window(perc_U,u1,u2,delta_U); 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the hanning window for the frequency integral  (Mallick an Frazer (1987),Figure 5a)

window_frequency = hanning_window(perc_F,f1,f2,delta_F);

%hanning_frequency(f1,f2,delta_F,nSamples);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% source parameters (Muller (1985), Equation 62)

phi = 0;   % receivers are assumed in line 
Z = 0;     % receivers are assumed on the surface
epslon1 = F3;
epslon2 = F1 * cos(phi) + F2 * sin(phi);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some usefull constants

cte = 1 / (4 * pi * rhom);
pi2 = pi * 2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the frequency of reference to normalize the complex slowness

if f1 > 7.5
      wRef = f1 * pi2;
   else
      wRef = 7.5 * pi2;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loop over all frequencies
iF = 1;
for f = f1:delta_F:f2
    disp ('Computation started at frequency'); f
    % setting some auxiliary arrays
    aux11(1:n_rec) = 0;
    aux12(1:n_rec) = 0;
    aux21(1:n_rec) = 0;
    aux22(1:n_rec) = 0;
    aux11old(1:n_rec) = 0;
    aux12old(1:n_rec) = 0;
    aux21old(1:n_rec) = 0;
    aux22old(1:n_rec) = 0;

     % some frequency related variables
     w = pi2 * f;                                       % angular frequency
     wC = complex(w, -tau);                  % complex frequency
	 wCR = abs(wC);                               % module of complex frequency
	 wCP = angle(wC);                            % phase of complex frequency
	 dUC = complex(w*delta_U/wCR,tau*delta_U/wCR);% complex slowness step
	 wCRwR = wCR / pi2;                       % assuming a rerence frequency = 1
	 wCCte = wC * cte;                           % an auxiliary variable

     % the frequency dependent slowness squared 
     [PSlowness,SSlowness,S2Velocity] = slowness(nL,Qp,Qs,wCRwR,wCP,alpha,beta);
         
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     % loop over all slowness
     iu=1;
     for u = u1:delta_U:u2
        
        % some slowness related variables 
        uC = complex(u,u * tau / wRef);  % complex slowness--Actually 'uC' is referred to as 'radial slowness'
        uC2 = 2 * uC;                      % 2 X complex slowness
        uuC = uC * uC;                   % complex slowness squared
	    uuC2 = 2 * uuC;                 % 2 X complex slowness squared    
	    muC = uC * -1;                  % -1 X complex slowness
        
        % the vertical slowness (Muller (1985), Equation (15))
        a = sqrt(PSlowness - uuC);
        b = sqrt(SSlowness - uuC);
        
        % computation of the reflectivity matrix (From the bottom to the source level "R^-" in Muller 1985)
        rm = Rm_matrix(nL,a,b,rho,S2Velocity,uuC,uC2,uC,thick,wC,z_Source);
        % free-surface boundary conditions. 
        %rp = free_surface(a,b,SSlowness,uuC2,uuC,uC,wC,z_Source);
        % The line above has been moved into the "if--else'' loop for selecting modelling multiple,
        %  it is logically not needed to compute this matrix only if multiples are simulated. (Y. Ma)
                      
        % reseting some variables
	    As1 = complex(0,0);      As2 = complex(0,0);      % downgoing waves
	    Cs1 = complex(0,0);      Cs2 = complex(0,0);      % downgoing waves
	    Bs1 = complex(0,0);       Bs2 = complex(0,0);      % upgoing waves
	    Ds1 = complex(0,0);      Ds2 = complex(0,0);      % upgoing waves

	    % P-wave potential
        am = a(1);
        bm = b(1);
        amI = complex(-imag(am),real(am));  % aml=i*am (i-- imag. unit)
        bmI = complex(-imag(bm),real(bm)); % bml = i*bm
        
        % some auxiliar quantities
        ambm = am * bm;
        aux1 = uuC / bm;
        aux2 = uuC / am; 
        
        % source amplitude vectors for Equation (72) (Muller (1985)
        As1 = uC;
	    As2 = complex(imag(aux2),-real(aux2));
	    Cs1 = aux1;
        Cs2 = complex(-imag(uC),real(uC));
        if isempty(Cs2) % If Cs2 is empty, it will cause some problem in matrix v2 (demension mismatch)
            Cs2 = 0;
        end
        Sd1 = [As1 Cs1]';
        Sd2 = [As2 Cs2]';      
        if direct==1
            Bs1 = muC;
            Bs2 = As2;
            Ds1 = aux1;
            Ds2 = -Cs2;
            Su1 = [Bs1 Ds1]';
            Su2 = [Bs2 Ds2]';
        end
        
        % computing compensation for free-surface auxiliar quantities
	    aux1 = uC2 * S2Velocity(1);
        aux2 = 1 - uuC2 * S2Velocity(1);
        aux2aux2 = aux2 * aux2;
        auxm1 = 4 * uuC * ambm * S2Velocity(1) * S2Velocity(1);
        aux3 = 1 / (auxm1 + aux2aux2);
        aux4 = aux2 * aux3;

        % computing the H matrix  for P-SV waves (Muller 1985, (89))
	    h(1,1) = ambm * aux1 * aux3;
        h(1,2) = bm * aux4;
        h(2,1) = am * aux4;
        h(2,2) = -h(1,1);
        
        % computing phase shift (that's the matrix G in Muller's  (Equation 87, Muller, 1985)
	    g(1) = exp(- z_Source * wC * amI);
	    g(2) = exp(- z_Source * wC * bmI);
        
        % computing Equation (76), Muller (1985)
        v1 = rm * Sd1;       %R-*Sd (Muller, 1985)
        v2 = rm * Sd2;
        if direct == 1
            v1 = v1 + Su1;
            v2 = v2 + Su2;
        end
        if mult == 1  % Reverberations
            % free-surface boundary conditions (reflection of free surface)
             % (Formulas, see Aki and Richards, 2002, pg. 134-136)
            rp = free_surface(a,b,SSlowness,uuC2,uuC,uC,wC,z_Source,alpha,beta);
            irr = inv(eye(2) - rm * rp);  % inv(I - R-R+)
            v1 = irr * v1;
            v2 = irr * v2;
        end
        
        % applying phase-shift
        v1 = v1 .* g';
        v2 = v2 .* g';
       
        % multiplication by matrix h
       v1 = 2.0*h * v1;
       v2 = 2.0*h * v2;
        
        % loop over offsets for computing the displacements
        for iR = 1 : n_rec
            arg = abs(uC * wC) * abs(geometry(iR));
            [J0,J1] = bessels(arg);
  
           %J0 = besselj(0,arg);   Using Matlab internal Function 'Besselj'
           %J1 = besselj(1,arg);   These functions are not computation
           %efficient with with comparable accuracy as 'bessels' gives
           
           % radial component
           
	       aux1 = -J1 * window_slowness(iu) * v1(1);
           aux11(iR) = aux11(iR) + aux1 + aux11old(iR);
	       aux11old(iR) = aux1;
           aux11(iR) = aux11(iR) + aux1;
	       
	       aux1 = J0 * window_slowness(iu) * v2(1);
           aux21(iR) = aux21(iR) + aux1 + aux21old(iR);
	       aux21old(iR) = aux1;
           
           % z component
           
	       aux1 = complex(-imag(v1(2)),real(v1(2)));
           aux1 = J0 * window_slowness(iu) * aux1;
           aux12(iR) =  aux12(iR) + aux1 + aux12old(iR);
	       aux12old(iR) = aux1;	
                     
	       aux1 = complex(-imag(v2(2)),real(v2(2)));
           aux1 = J1 *window_slowness(iu) * aux1;
           aux22(iR) = aux22(iR) + aux1 + aux22old(iR);
	       aux22old(iR) = aux1;
        end
        iu = iu + 1;
     end
    % computing the displacement
	dUCEp1 = epslon1 * dUC;
	dUCEp2 = epslon2 * dUC;

    % loop over offsets for computing the displacements
    for iR = 1 : n_rec
        % displacements in the radial direction (frequency domain)
	    auxm1 = aux11(iR) * dUCEp1;
        auxm2 = aux21(iR) * dUCEp2;
	    uRw(iF,iR) = (auxm1 + auxm2) * wCCte * window_frequency(iF) * sign(geometry(iR));
                
        % displacements in the vertical direction (frequency domain)
        auxm1 = aux12(iR) * dUCEp1;
        auxm2 = aux22(iR) * dUCEp2;
        uZw(iF,iR) = (auxm1 + auxm2) * wCCte * window_frequency(iF);
    end
    iF = iF + 1;
end
%zw = real(uZw);
%rw = real(uRw);
%figure;
%plot(uRw);title=('Complex Radial component')
%figure;
%plot(uZw);title=('Complex Vertical component')
for n=1:n_rec
    uRc(:,n) = ifft(uRw(:,n),nSamples);
    uZc(:,n) = ifft(uZw(:,n),nSamples);
end

uR = real(uRc);
uZ = real(uZc);

t=[0:delta_T:T_max];
uR2 = uR;
uZ2 = uZ;
for n = 1:n_rec
   for m = 1:nSamples
       uR2(m,n) = uR2(m,n) * exp( tau * m * delta_T);
       uZ2(m,n) = uZ2(m,n) * exp( tau * m * delta_T);
   end
end