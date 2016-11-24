function dtstep=maxtimestep(dx,vel,laplacian)
% MAXTIMESTEP ... compute the maximum stable time step
%
% dtstep=maxtimestap(dx,vel,laplacian)
%
% dx ... the spatial grid spacing
% vel ... the velocity model (matrix)
% laplacian ... either 1 or 2 for 2nd or 4th order.
%


vmax=max(vel(:));
if laplacian ==1
    
    dtstep=dx/(sqrt(2)*vmax);

elseif laplacian==2

    dtstep=sqrt(3/8)*dx/vmax;
    
else
    error('laplacian can only be 1 or 2')

end