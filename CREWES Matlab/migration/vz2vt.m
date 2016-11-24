%  VZ2VT: Compute V(x,t) and Vrms(x,t) from V(x,z) and a ZOS exploding
%         reflector model of V(x,z)
%
%  This function takes in a depth velocity model and a ZOS exploding
%  reflector model based on the velocity model and computes a velocity
%  matrix in time and an rms velocity matrix in time
%
%  This function is useful for converting depth models to time velocity models
%  to test the capabilities of the CREWES migration routines on ZOS images
%
%  [vmat,vrmsmat,tmat,xsamp,tsamp]=vz2vt(vel,seis,samprates)
%
%  vel..........is the input velocity model in depth V(x,z)
%  seis........the input exploding reflector model
%  samprates....is a vector of the form [dz,dx,dt], where dz and dx are the
%               model spatial grid steps in meters, and dt is the sample rate of the
%               ZOS exploding reflector model in seconds
%               eg.  [5,5,.0005]
%
%               these should co-respond to the values used to build the 2
%               models
%
%  tmat...... time coordinate matrix. Each column contains the times
%             for each trace at depth
%  vmat...... is the output resampled velocity matrix in time, V(x,t) in
%             2way travel time
%  vrmsmat....is the output RMS velocity matrix in time, Vrms(x,t)
%  xsamp.......is the vector of horizontal grid positions for V(x,t),Vrms(x,t)
%  tsamp.....is the vector of timesamples used for V(x,t), Vrms(x,t)
%
%  Zoron Rodriguez, November 2006
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

function [vmat,vrmsmat,tmat,xsamp,tsamp]=vz2vt(vel,seis1,samprates)


if (nargin<3)
error('Not enough inputs please use the correct number of input including sample rates [dx,dz,dt]')
end
if (length(samprates)<3)
error('Sample rates must be a 1x 3 vector of form [dx,dz,dt] in meters,meters and seconds')
else
dz=samprates(1);
dx=samprates(2);
dt=samprates(3);
end

[sz,sx]=size(seis1); % dimensions of the seismic exploding reflector model
[vz,vx]=size(vel);    %dimensions of the velocity model matrix

if (vx~=sx)
    error('Exploding reflector model "seis" must have the same number of columns as the velocity model matrix "vel" that created it')
end

tstart=clock;     %save start time

%%%%%%%%%%%%%%Step 1 calculate interval travel times matrix%%%%%%%%%%
%note that the size of the time matrix is 1 row of samples longer
%than the velocity matrix 400x400-> 401x400
%this is due to the function vint2t calculating traveltimes to each point
%starting from zero at the surface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[vz,vx]=size(vel);    %dimensions of the velocity model matrix
z=dz*(0:vz);          % depth vector
xsamp=dx*(0:vx-1);     %x-positions returned to output
tmat=zeros(vz+1,vx);  %storage for time-samples

disp('Calculating Two-way vertical travel times ');

for s=1:vx
 t1=vint2t(vel(:,s),z); % converts to vertical travel time from instantaneous
 tmat(:,s)=t1;
end

%tmat1=tmat;  % store 1-way times
tmat=2*tmat;  %change to 2-way traveltimes

t_travtime=clock;  % time to finish calculating traveltimes
t_elapsed=etime(t_travtime,tstart); %elapsed time
disp(['Two-way vertical travel times calculated in ', num2str(t_elapsed),' seconds']);

t_resamp=clock;  %get the current time
disp('Resampling the velocity matrix from depth to time');
%%%%%%%%%%%%%%%%%%Step 2 Resample the velocity matrix %%%%%%%%%%%%%%%%%%
%
%   Resamples the matrix to instantaneous velocity as a function of time
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
vmat=2800*ones(size(seis1));   %storage for resampled velocities
[sz,sx]=size(seis1);           %get the dimensions of the seismic matrix
tsamp=dt*(0:sz-1);             %make a vector of time samples
vsamp=zeros(size(tsamp));      % temporary storage array for velocities

for s=1:sx                                  %loop over traces
     vsamp(1)=vel(1,s);    %fill the first entry
     k=2;                  %start at the second entry (array size mismatch)
    for p=2:sz                              %loop over samples
         if k<=vz
            if  tsamp(p)<= tmat(k,s) %if the sample is less than traveltime
                vsamp(p)=vel(k-1,s); %to the next layer keep velocity same
            else
                k=k+1;               %else change to the next velocity
                vsamp(p)=vel(k-1,s); %
            end
         else
             vsamp(p)=vel(k-2,s);    %if you run out of samples just fill
         end                         %with the last velocity sample
    end

    vmat(1:length(vsamp),s)=vsamp;   %store in a resampled velocity matrix

    if (rem(s,50)==0)
        t_elapsed=etime(clock,t_resamp); %  get the elapsed time
        disp(['Column ', num2str(s), ' of ', num2str(sx), ' resampled to time in ', num2str(t_elapsed),' seconds']);
    end
end
disp('Resampling completed, now calculating RMS velocities');

%%%%%%%%%%%%Step 3 calculate corresponding RMS velocity matrix%%%%%%%%%
%
%      calculate RMS velocities at each time
%      to be completed later
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
t_rms=clock; % get the current time
[vz,vx]=size(vmat); % get the size of the resampled velocity matrix
vrmsmat=zeros(size(vmat)); %storage array for rms velocities

for s=1:vx              % loop over traces
    vrms=vint2vrms(vmat(:,s),tsamp);   %calculate the RMS velocity
    vrmsmat(:,s)=vrms;                 %store

    if (rem(s,50)==0)
        t_elapsed=etime(clock,t_rms); %  get the elapsed time
        disp(['RMS velocities calculated for Column ', num2str(s), ' of ', num2str(sx), '  in ', num2str(t_elapsed),' seconds']);
    end
end

t_elapsed=etime(clock,tstart); %elapsed time
disp(['V(x,t) and VRMS(x,t) calculated in ', num2str(t_elapsed),' seconds']);