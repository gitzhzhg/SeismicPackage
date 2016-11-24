function indtraj=ind2traj(irow,icol,nrows)
%
% indtraj=ind2traj(irow,icol,nrows)
%
% IND2TRAJ converts a set of row and column indicies to a set
% equivalent 1-D indicies which evaluate the matrix along the
% trajectory specified by the row/column indicies. Given irow
% as a vector and icol as a vector, direct evaluation of a 
% A as: B=A(irow,icol), will result in B as a matrix which
% has dimensions length(irow) by length(icol). However, there
% is an alternate interpretation of irow and icol as a set of
% index pairs which specify a trajectory through A. In this
% case, we would like to write something like: B=A(indtraj)
% and have B result as a vector which is the matrix A sliced
% along the trajectory. This routine does jsut that.
%
%	irow ... vector of row indicies of the trajectory
%	icol ... vector of column indicies of the trajectory
%	nrows ... number of rows of the matrix to be sliced
%	indtraj ... index of the trajectory
%
% example: A=magic(3);
%	indtraj=ind2traj(1:3,1:3,3);
%	A(indtraj) % returns the main diagonal of A
%
% G.F. Margrave, University of Calgary, 1996
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
if(length(icol)~=length(irow))
	error('irow and icol must be the same length');
end
indtraj = (icol(:)-1)*nrows + irow(:);