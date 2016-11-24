function M=afd_movie(dx,dtstep,tmax,velocity,snap1,snap2,laplacian,maxframes,wavelet)
% AFD_MOVIE make movies of wavefield propagation
%
% M=afd_movie(dx,dtstep,tmax,velocity,snap1,snap2,laplacian,maxframes,wavelet)
%                 
% AFD_MOVIE creates a movie of the snapshots of the wavefield as it 
% propogates through time to the desired output time.  Two input matrices 
% of the wavefield, one at time=0-dtstep and one at time=0, are used in a 
% finite difference algorithm to propogate the wavefield.  The finite 
% difference algorithm can be calculated with a five or nine point 
% approximation to the Laplacian operator.  The five point approximation 
% is faster, but the nine point results in a broader bandwidth.  The movie
% of the propoagating wavefield is returned.  The number of frames determines
% the time spacing between snapshots, but the time spacing will never be
% less than the value of dtstep.  Note that the velocity and grid 
% spacing must fulfill the equation max(velocity)*dtstep/dx > 0.7 
% for the model to be stable.  This condition usually results in 
% snap1 and snap2 being identical.    
%
% xmax = the maximum horizontal length of the grid (in consisent units)
% zmax = the maximum vertical length of the grid (in consistent units)
% dx = the bin spacing for both horizontal and vertical (in consistent units)
% dtstep = time interval in seconds
% velocity = the input velocity matrix in consisnent units
%          = has a size of floor(zmax/dx)+1 by floor(xmax/dx)+1
% snap1 = the wavefield at time=0 - dtstep (same size as velocity matrix)
%        = will be based on the source array desired i.e. the position
%          of the sources will be one, and the rest of the positions
%          will be zero
% snap2 = the wavefield at time = 0 (same size as velocity matrix)
% tmax = the time at which the propogated wavefield is
%           returned (seconds)
% maxframes = the number of frames or snapshots to appear in the movie
%            (it is suggested that there should not be more than
%            40 snapshots in one movie)
% laplacian - an option between two approximations to the laplacian operator
%           - 1 is a 5 point approximation
%           - 2 is a nine point approximation
% boundary = indicate whether all sides of the matrix are absorbing
%          = '1' indicates all four sides are absorbing
%          = '2' choses three sides to be absorbing, and the top one not to be
%             this enables sources to be put on the surface
% wavelet = source waveform. It must be sampled at the rate dtstep.
%
% snapshotn = the wavefield propagated forward to the specified time
%
% by Carrie Youzwishen, February 1999
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

boundary=2;

[nz,nx]=size(snap1);
if(prod(double(size(snap1)~=size(snap2))))
	error('snap1 and snap2 must be the same size');
end
if(prod(double(size(snap1)~=size(velocity))))
	error('snap1 and velocity must be the same size');
end

xmax=(nx-1)*dx;
zmax=(nz-1)*dx;

x=0:dx:xmax;
z=(0:dx:zmax)';

nx=floor(xmax/dx)+1;
nz=floor(zmax/dx)+1;

% spatial coordinates
t=0:dtstep:tmax-dtstep;		t=t';

if laplacian ==1 
    if max(max(velocity))*dtstep/dx > 1/sqrt(2)
    	error('Model is unstable:  max(velocity)*dtstep/dx MUST BE < 1/sqrt(2)');
    end
elseif laplacian ==2
    if max(max(velocity))*dtstep/dx > sqrt(3/8)
    	error('Model is unstable:  max(velocity)*dtstep/dx MUST BE < sqrt(3/8)');
    end
else
   error('invalid Laplacian flag')
end

nsteps=round(tmax/dtstep)+1;

if maxframes > nsteps
	disp('The maximum number of frames is larger than the possible number');
	disp('of frames.  The program will default to the possible number');
	maxframes=nsteps;
end

interval=floor(nsteps/maxframes);

if (interval == 0)
   interval=1;
end

iframes=1:interval:nsteps;

nframes=length(iframes);

%M=moviein(nframes);

jframe=1;
time0=clock;
%maxstep=round(tmax/dtstep)-1;
disp(['There are ' int2str(nsteps) ' steps to complete']);
nwrite=2*round(nsteps/50)+1;
sources=snap2;
snap2=wavelet(1)*sources;
amax=0;
for k=1:nsteps

   [snapshotn]=afd_snap(dx,dtstep,velocity,snap1,snap2,laplacian,boundary);
	if (k == iframes(jframe))
        if(~amax) amax=max(max(abs(snapshotn))); figure; end
        tmp=(1+.6*snapshotn/amax).*velocity;
		imagesc(x,z,tmp);
        colormap(flipud(gray))
		%M(:,k/interval)=getframe;
		M(jframe)=getframe;
		jframe=jframe+1;
        if(jframe>nframes)
            return;
        end
		%close(gcf);		
	end
   snap1=snap2;
   snap2=snapshotn;
   if(k<=length(wavelet))
       snap2=snap2+sources*wavelet(k);
   end

    if rem(k,nwrite) == 0
       timenow=clock;
       tottime=etime(timenow,time0);

       disp(['wavefield propagated to ' num2str(k*dtstep) ...
       ' s; computation time left ' ...
        num2str((tottime*nsteps/k)-tottime) ' s']);
    end 


end