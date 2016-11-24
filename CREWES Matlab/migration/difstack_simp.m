%  DIFSTACK_SIMP: create a diffraction stack from vrms(x,t) and a ZOS image
%
%  Simplified version of a diffraction stack algorithms without parameter
%  choices. The full version is available as dif_stack.m.
%
%  This function takes in a ZOS exploding reflector image and sums along
%  hyperbolas T^2=T0^2+4x^2/V^2 where V=vrms(x,t) and T0= the time position
%  of the current sample. the output is the migrated stack.
%
%  This code is based upon the algorithm of J. Bancroft pg 4.14 in his book
%  "A Practical Understanding of Pre and Post Stack Migration" (2006)
%
%
%
%  [migdata,tsamp,xsamp]=difstack_simp(ERM_seis,vrmsmat,dt,dx)
%
%  ERM_seis.....the input exploding reflector model
%  vrmsmat......RMS velocities Vrms(x,t) from vz2vt from 2way times
%  dt...........time sample rate (eg.  0.0005)
%  dx...........spatial sample rate(eg.  5)
%               these should co-respond to the values used to build
%               the velocity model and the ZOS image
%
%  migdata......the migrated seismic stack
%  xsamp........is the vector of horizontal grid positions for V(x,t),Vrms(x,t)
%  tsamp........is the vector of timesamples used for V(x,t), Vrms(x,t)
%
%  Zoron Rodriguez, John C. Bancroft,  November 2007
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

function [migdata,tsamp,xsamp]=difstack_simp(seis1,vrmsmat,dt,dx)

if (nargin<4)
  error('Not enough inputs, type help difstack and re-read instructions');
end
[vt,vx]=size(vrmsmat);
[st,sx]=size(seis1);
if ([vt,vx]~=[st,sx])
   error (' RMS velocity matrix must have the same dimensions as the exploding reflector model both in columns (traces) and rows (time samples)')
end
migdata=zeros(size(vrmsmat));  % creat a storage array for the migrated section
A=sx;   % arperture for sumation
xsamp=dx*(0:vx-1); %return for plotting
tsamp=dt*(0:vt-1); %return for plotting

%%%%%%%%%%%%%%%%%%%%%%%%Migration Loop%%%%%%%%%%%%%%%%%%%%%
t_migloop=clock;  %get the current time before migration starts
disp('Migrating data by diffraction stack method')
for t=1:vx      %loop over traces
    t_trace=clock;  % get clock time for the current trace

    for s=1:vt   %loop over samples in the trace
        V=vrmsmat(s,t); % grab the local vrms at that position
        To=s*dt;       %define diffaction apex time
        sum=0;         %initialize sum
        count=0;       % counter for averaging the value of the sum by the number of samples in the sum
        for n=-A:A  % loop over the migration aperture
            x=n*dx; % move out by n steps
            T=sqrt((To)^2+(2*x/V)^2); % calculate the time T at movout x
            m=round(T/dt); % take the floor or ceilling of this point as the time sample position to take
            k=t+n;  %start at the trace + the moveout number of samples
            if (m<vt&k>0&k<=sx) %check bounds
                W=To/T;    %weight the value of the diffraction point
                sum=sum+W*seis1(m,k);  %cumulative sum
            end
            count=count+1;  % increment the counter of the number of samples  in the sum
        end
            migdata(s,t)=sum/count; %put the sum at the apex of the diffraction and average by the number of samples in the sum

    end
       if (t==1|t==10|rem(t,25)==0)
         t_elapsed=etime(clock,t_migloop); %  get the elapsed time
         disp(['Positions migrated for Trace ', num2str(t), ' of ', num2str(vx), '  total time elapsed is ', num2str(t_elapsed),' seconds with ', num2str((vx/t-1)*t_elapsed),' seconds remaining']);
       end
end

t_elapsed=etime(clock,t_migloop); %  get the elapsed time
disp(['Diffraction stack migration completed in ', num2str(t_elapsed),' seconds']);

end