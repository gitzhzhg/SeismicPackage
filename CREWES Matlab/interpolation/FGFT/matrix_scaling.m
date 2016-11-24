function Y = matrix_scaling(X, nx2, ny2)
% This function projects the values of an [nx1 by ny1] array into an array
% of size [nx2 by ny2] using the nearest neighbor interpolation.
%
%% Input
%   X: The original array
%   nx1: number of samples in x direction for Output matrix
%   ny1: number of samples in y direction for Output matrix
%
%% Output
%   Y: Output matrix
%
% Author: Mostafa Naghizadeh; Copyright (C) 2010
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


% Determining the size of the original data
[nx1,ny1]=size(X);

% Vectors to determine the x,y coordinates of the new matrix related to old grid
XC=round((nx1/nx2)*[1:nx2]);
YC=round((ny1/ny2)*[1:ny2]);

% Make sure indices of matrix are matched
for ic=1:nx2
    if (XC(ic)==0)
        XC(ic)=1;
    end
    if (XC(ic)>nx1)
        XC(ic)=nx1;
    end
end
for ic=1:ny2
    if (YC(ic)==0)
        YC(ic)=1;
    end
    if (YC(ic)>ny1)
        YC(ic)=ny1;
    end
end

% Replacing values of X into Y
Y=X(XC,YC);