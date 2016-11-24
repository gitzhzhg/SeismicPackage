function [wavefield,z,x]=afd_source(xmax,zmax,delx,nsource,xsource,zsource,sz,default,smatrix)
% AFD_SOURCE ... generates a source array for uses with AFD_SEISMO
% 
% AFD_SOURCE generates a source array within a grid of extent
% zmax by xmax.  The default setting creates source arrays made up
% of "point" sources one bin in size.  The size of an array 
% and placement within the matrix are determined by the user.
% One may also choose to input a square source matrix, or matrices.
% The center of these matrices will be placed at the specified position,
% unless the array is positioned near the top of the matrix, in which
% case they will be shifted to fit within the grid.  .  
% Source arrays can overlap.  
%
% xmax = the maximum horizontal length of the grid (in consisent units)
% zmax = the maximum vertical length of the grid (in consistent units)
% delx = the bin spacing for both horizontal and vertical (in consistent units)
% nsource = the number of source arrays
% xsource = a vector of the center x positions of the source arrays (in consistent units)
%           a value of x=0 represents a source at the left hand side
% zsource = a vector of the center z positions of the source arrays (in consistent units)
%           a value of z=0 represents a source on the surface
% sz = a vector of the size of each source array (in consistent units)
%      **when using the point source default this is the horizontal extent
%       in meters or other applicable units of the source array
%      **when inputting a square matrix this is the size of the matrix
%      in bins where bin# = floor(x-extent/delx)+1 OR = floor(z-extent/delx)+1
%      NOTE sz must be an odd number when inputting a matrix, so that it may
%           be centered
% default = a parameter which specifies whether the source array is 
%           specified using the program parameters, or entered as a matrix
%           '1' turns the default on and the array is built within the program
%           '0' turns it off, and enables the entry of source matrices
% smatrix = a vector of the square matrices i.e. [matrix1 matrix2]
%           where the size of these matrices is defined by 'sz'
%           **if using the default setting, set smatrix=0
%
% wavefield = the initial snapshot of the source array
%
% by Carrie Youzwishen  February 1999
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

tic;

%convert dimensions to number of bins
nx=floor(xmax/delx)+1;
nz=floor(zmax/delx)+1;

%Spatial coordinates
z=0:delx:zmax;
x=0:delx:xmax;

%convert position of sources to bin positions
xsource = floor(xsource./delx)+1;
zsource = floor(zsource./delx)+1;

if xsource>nx
disp('Error: The position of the source is outside the matrix');
return;
end

if zsource>nz
disp('Error: The position of the source is outside the matrix');
return;
end

if max(size(xsource)) ~= nsource
disp('Error:  the number of positions in "xsource" and the');
disp('number of sources do not match!');
return;
end

if max(size(zsource)) ~= nsource
disp('Error:  the number of positions in "zsource" and the');
disp('number of sources do not match!');
return;
end

wavefield=zeros(nx,nz);

if default == 1

   %convert horizontal extent of source array to bins rather than meters
   sz=floor(sz./delx)+1;

   
   if max(size(sz)) ~= nsource
   disp('Error:  the number of sizes in "sz" and the');
   disp('number of sources do not match!');
   return;
   end

 

   wavefield=zeros(nz,nx);
   for k=1:nsource
                    if xsource(1,k)-(round(sz(1,k)/2)-1) < 1
                          disp(['Error: source ' num2str(k) ' does not fit within the matrix']);
                          return;
                    end 

                    if xsource(1,k)+(round(sz(1,k)/2)-1) > nx
                          disp(['Error: source ' num2str(k) ' does not fit within the matrix']);
                          return;
                    end 

                    wavefield(zsource(1,k),xsource(1,k)-(round(sz(1,k)/2)-1):xsource(1,k)+....
		    (round(sz(1,k)/2)-1))= 1+ wavefield(zsource(1,k),xsource(1,k)-....
                    (round(sz(1,k)/2)-1):xsource(1,k)+(round(sz(1,k)/2)-1));
                    disp(['Source ' num2str(k) ' has been built']);

   end

else

  % check that size is an odd number
  if rem(sz,2) == 0
        disp('Error: Size must be an odd number in order to center the source matrix');
        return
  end
  
  for k=1:nsource
     
    extent=round(sz(1,k)/2)-1;
    
        % check if source matrix should be centered at zsource,xsource

        if xsource(1,k) - extent < 1
            disp('Error: The source matrix is too wide horizontally to be centered at xsource');
            return;
        end

        if zsource(1,k) - extent >= 1

                  wavefield(zsource(1,k)-extent:zsource(1,k)+extent,...
                  xsource(1,k)-extent:xsource(1,k)+extent) = smatrix;
                  disp(['Source matrix  ' num2str(k) ' has been placed in matrix']);

        else  
                  wavefield(zsource(1,k)-extent+abs(zsource(1,k)-extent) ....
                  + 1:zsource(1,k)+extent+abs(zsource(1,k)-extent)+1, ....
                  xsource(1,k)-extent:xsource(1,k)+extent) = smatrix;
                  disp(['Source matrix ' num2str(k) ' has been placed in matrix']);

        end

  end

end

toc;