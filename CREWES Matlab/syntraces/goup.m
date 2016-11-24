function [pm,p,rcs]=goup(rin,fpress,isrc,irec)
% GOUP: compute the complete response (primaries + multiples) of a 1D layered system.
%
% [pm,p,rcs]=goup(r,fpress,isrc,irec)
%
% GOUP computes a 1D seismic response by the Goupillaud algorithm as given
% in:
%	Waters, "Reflection Seismology", 1981, John Wiley, pp 128-135
% The model is essentially a layered sequence above a half space. You
% provide the reflection coeficients for incidence from above on the top of
% each layer (the half space is considered as a layer). According to the 
% Goupillaud model, all layers should have the same vertical traveltime dt. 
% This traveltime is not an imput parameter but it essentially becomes the 
% time sample interval of the output time series. So, create your input model
% such that there is one interface every dt seconds where dt is, say, .001 
% seconds. In GOUP, dt is considered to be the two-way traveltime across a layer
% and the output seismogram is sampled at dt. This means that the time for 
% a downgoing wave to move from interface j to j+1 is dt/2. Consequently,
% if abs(isrc-irec) is an odd number, then the times of the output seismogram 
% fall on a grid that is shifted by dt/2 relative to the case when
% abs(isrc-irec) is even. So, in the even case, the first sample of the
% output is a t=0, while in the odd case it is at t=dt/2. To avoid
% confusion, try to make abs(isrc-irec) an even number.
%
% Note: The output (pm and p) are the same length as the input r. This
% means that if you want to see the multiple train far after the last
% primary, you should pad your primary reflection coefcients with zeros.
%
% Note: The source is taken to initiate a unit pulse at time zero. In the
% case of a pressure seismogram (modelling a hydrophone) then both the up
% and downgoing waves have magnitude +1 at the source. For the same source
% but a displacement seismogram, the up and downgoing waves have opposite
% signs with the former being -1 and the latter +1.
%
% r ... input reflection coeficients, regularly sampled in TIME.
%       r(j) is at two way time of 2*(j-1)*dt where dt is the time sample rate.
%       These are the reflection coeficients for incidence from above.
% fpress ... flag for a pressure or displacement seismogram
%            1 -> a pressure seismogram is desired (hydrophone)
%            0 -> a displacement seismogram is desired (geophone)
%      ******* default = 0 ******
% isrc ... index giving source depth. isrc=1 means the source is at
%       interface 1 (i.e. the surface).
%      ******* default = 1 ******
% irec ... index giving receiver depth. irec=1 means the receiver is at
%       interface 1 (i.e. the surface)
%      ******* default = 1 ******
% NOTE: Both sources and receivers are assumed to be directly on
%      interfaces. So a source on interface 3 emits a unit pulse into both
%      layers 2 and 3. If fpress=1, then both upgoing and downgoing pulses
%      are +1; however, if the fpress=0, then the downgoing pulse is +1 but
%      the upgoing is -1.
% pm ... output 1-D impulse response, primaries plus multiples,
%	sampled at the same times as r. This is the sum of the upgoing and
%	downgoing waves SCATTERED FROM the recording interface.
% p ... primaries only showing the effects of transmission losses.
%   This is simply the upgoing wave (without multiples) as INCIDENT ON the 
%   recording interface.
% rcs ... output reflectivity. This is the same as the input r if
%   isrc=irec=1, but is different otherwise because the traveltimes are
%   different.
%
% NOTE: The difference between p and rcs is the transmission loss. To plot
% the transmission loss us plot(p./(rcs+100*eps))
%
% NOTE: pm-p gives the multiples in the seismogram.
%
% NOTE: if r(1)==1, this means the upper interface is a free surface. Since
% pressure vanishes at a free surface, then fpress=1; irec=1; will result
% in pm=[0 0 0 0 ...]. (p will not vanish but pm will).
%
% 
% G.F. Margrave, CREWES, May 2011
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

if(nargin<2)
    fpress=1;
end

if(nargin<3)
    isrc=1;
end
if(nargin<4)
    irec=1;
end

if(fpress~=1 && fpress~=0)
    error('invalid value for fpress')
end

if(isrc>length(rin) || irec>length(rin))
    error('isrc and irec must not be greater than length(r)')
end
if(isrc<1 || irec <1)
    error('isrc and irec must not be less than 1')
end

factor=1;
if(fpress==0)
    factor=-1;
end
% factor = 1-2*(fpress-1);
% fpress=1 -> factor =1; (pressure)
% fpress=0 -> factor = -1; (displacement)
%

%initialize a few things
r=rin(2:length(rin));
r0=rin(1);
d=zeros(size(rin));
d(isrc)=1; %initial downgoing wave
pm=zeros(size(rin));
p=pm;
rcs=pm;

%
% -------------- interface 1
%                layer 1
% -------------- interface 2
%                layer 2
% -------------- interface 3
%                layer 3

% propagate upgoing direct wave from source all the way to the surface and
% generate the downgoing waves from each interface above the source
u=1;%initial direct wave from source
if(fpress==0)
    %an explosion generates an upgoing negative displacement and a downging 
    %positive displacement
    u=-1; 
end
tnot=round(abs(isrc-irec)/2)+1;%arrival time of direct wave at receiver
for k=isrc-1:-1:1
    d(k)=-factor*rin(k)*u;%reflected u becomes downgoing
    uin=u;%incident u
    if(k==1)
        u=0;%no transmitted wave above the free surface
    else
        u=(1+rin(k))*uin;%transmitted u
    end
    if(k==irec)
        pm(tnot)=d(k)+uin;%record sum of both waves on one side at receiver
        %p(tnot)=uin;%primary means just the incident upgoing wave.
    end
end
% Diagram for above loop:
%    ---------- isrc-2
%         / \ d(isrc-2)
%       u/
%    -------- isrc-1    Diagram shows source at * and direct upgoing wave
%   uin/ \ d(isrc-1)     as / .  At each interface above *  a downgoing wave 
%     /                 is generated.
%    *---- isrc


%include direct wave if needed
if(isrc==irec)
    pm(1)=d(isrc);
    %p(1)=d(isrc);
end

%loop over upcoming waves. We get one upcoming wave from each interface
%below the source. The interface at isrc+1 give rise to the first reflected 
%upgoing wave.
%outer loop steps until downgoing direct wave from source reaches the lowest
%interface. The inner loop brings each downgoing wave to the surface.


for k=isrc:length(r)
	
	%zero upgoing wave at each step
	u=0.0;%upgoing primaries plus multiples
	up=0.0;%upgoing, primaries only wave

	%step from r(k) to the surface
    idown=0;
	for j=k:-1:1
    % here we step up from the reflector to the surface encoutering
    % different downgoing waves and different reflectors at each step.
    % The index j is basically the reflector number (j+1 to be precise),
    % while idown is counting downgoing waves. The outer loop index, k,
    % is the number of the upgoing wave. The first possible upgoing wave is
    % at the source, isrc, and earlier upgoing waves are not possible.
    idown =idown+1;
        
 % Diagram for present loop:
%    ----*---------- isrc
%         \
%      d(j)\  /u
%    --------------- j+1>isrc       Diagram shows source at * and upgoing or downgoing waves
%       uin/ \ d(j+1)             as / .  The direct downgoing wave generates an upcoming 
%         /   \                   wave at each interface.
%   ---------------- j+2      

		%update downgoing wave
        %d(j) is the downgoing wave in the jth layer
        uin=u;
		d(j+1)=(1-r(j))*d(j) -factor*r(j)*uin; %JKC 14feb/08
        
		%update upgoing wave     
		u=factor*r(j)*d(j) + (1+r(j))*uin; %JKC 14feb/08
        
        % see diagram above. If the receiver is at level j+1 
        % then we use the code below to record.
        
        %The time of any arrival is just the number of layers traversed by
        %the arrival on its total path (because the layers have equal
        %traveltime). The time is purely a function of the starting and
        %ending points on the Goupillaud (or computation) grid (all raypaths
        %with the same start and end points have identical times). 
        %(See figure 3.5 in "Numerical Methods of Exploration Seismology" by 
        %G.F. Margrave avaialbel at www.crewes.org.) Any point
        %on the grid has unique coordinates (uj,dj) which are the number
        %of an upgoing wave and a downgoing wave.  The downgoing wave from
        %the source is always zero while the upgoing wave from the source
        %has the same number as the depth of the source. Since we use index
        %1 to denote zero depth, there is no upgoing wave 0 but there is a
        %downgoing wave zero. The time to go from point (uj,dj) to (uk,dk)
        %is given by abs(uj-uk)+abs(dj-dk). Our events all start from
        %(isrc,0) and end at (k,idown)
        
        time_index=round((abs(isrc-k)+idown)/2)+1;%Add 1 because zero time is index 1
        
        if(j+1==irec) 
            %pm(time_index)=u+d(j+1)+d(j)+uin;%sum of all waves at the interface
            pm(time_index)=u+d(j);%sum of all waves on one side of the interface
            p(time_index)=up;%upgoing primary only
            rcs(time_index)=R;%reflectivity at the correct time
        end
        
        if( j==k ) %this block captures the primary
            up = u;
            R=r(j);
        else
            %up is the primary with transmission loss but no multiples
            up = (1+r(j))*up; %JKC 14feb/08
        end
        
	end

	%step to surface
    d(1)= -factor*r0*u;
    idown=idown+1;
    time_index=round((abs(isrc-k)+idown)/2)+1;
    if(irec==1)
        pm(time_index)=u+d(1);%upgoing plus surface multiple
        p(time_index)=up;%upgoing primary only
        rcs(time_index)=R;%reflectivity at the correct time
    end
	

end