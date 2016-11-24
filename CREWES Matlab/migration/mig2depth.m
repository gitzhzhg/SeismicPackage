%  MIG2DEPTH: Compute a depth image of a migrated section given the
%             velocity model in time V(x,t).
%
%  [zsamp,ztrmat]=mig2depth(migdata,vmat,tsamp,dz,dt)
%
%
%  migdata... the input migrated section in time
%  vmat...... is the instantaneous velocity model in time V(x,t)in
%             2way travel time
%  tsamp.....is the vector of timesamples used for V(x,t)
%  dz........depth sample rate of the model (eg. 5)
%  dt........time sample rate of the data in seconds (eg. 0.0005)
%
%  zsamp..... array of depths equally sampled by dz
%  ztrmat.... depth converted seismic section
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
function  [zsamp,ztrmat]=mig2depth(migdata,vmat,tsamp,dz,dt)
if (nargin<5)
    error('not enough inputs, type help mig2depth and re-read the instructions')
end

[sz,sx]=size(migdata); % dimensions of the migrated seimsic image in time
[vz,vx]=size(vmat);    %dimensions of the velocity model matrix

if (vx~=sx)
    error('Migdata must have the same number of columns as the velocity model matrix vmat')
end

tstart=clock;  % save starting time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                   Step 1:
%        Calculate time to depth curves
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[vt,vx]=size(vmat);
zmat=zeros(size(vmat));

for k=1:vx
    temp=dt*vmat(:,k);  %integrands
    temp(1)=0;     %set depth =0 at time 0
    zmat(:,k)=cumsum(temp); %integrate
end

disp(['Time to depth curves calculated in ',num2str(etime(clock,tstart)),' seconds']);
disp('Calculating the depth stretched section');
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                Step 2:
%  Calculate the depth stretched section
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
t_depstr=clock;% get the current time
% make a storage array the size of the first trace
% need to know the average size of a trace to do this
% so calculate the first one...
[ztrc,z]=time2depth(migdata(:,1),tsamp,[tsamp' .5*zmat(:,1)],dz);
ztrmat_temp=zeros(nextpow2(2*length(z)),vx);

%loop over traces
maxsize=0;
for k=1:vx
    % calculate succssive traces (.5 is for 2 way travel time converts back to 1 way depth)
    [ztrc,z]=time2depth(migdata(:,k),tsamp,[tsamp' .5*zmat(:,k)],dz);
    %[ztrc,z]=time2depth(trc,t,tzcurve,dz)
    ztrmat_temp(1:size(ztrc),k)=ztrc; %store in storage array

      %if statement to determine the maximum trace length for truncation
      if maxsize<=length(z);
      maxsize=length(z);
      end
      if (rem(k,25)==0| k==1|k==10)
      t_elapsed=etime(clock,t_depstr); %  get the elapsed time
      disp(['Trace ', num2str(k), ' of ', num2str(vx), ' depth converted in ', num2str(t_elapsed),' seconds']);
      end
end
% throw away extra samples
zsamp=dz*(0:maxsize-1);
ztrmat=zeros(maxsize,vx);
ztrmat=ztrmat_temp(1:maxsize,:);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(['Seismic section depth converted by simple stretch in ',num2str(etime(clock,tstart)),' seconds'])