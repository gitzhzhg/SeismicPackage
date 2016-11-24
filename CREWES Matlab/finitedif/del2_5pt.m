function [output]=del2_5pt(input,delx)
% DEL2_5PT ... compute the 5 point Laplacian
%
% [output]=del2_5pt(input,delx)
%
% DEL2_5PT computes the 5 point approximation to the laplacian operator of 
% the two dimensional matrix 'input'.  The horizontal and vertical bin
% spacing of the matrix MUST be the same.    
%
% input = input matrix
% delx = the horizontal/ vertical bin spacing in consistent units
%
% output = the output matrix
%
% by Carris Youzwishen, April 1999
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

[nrows,ncolumns]=size(input);

input2=zeros(nrows+2,ncolumns+2);
input2(2:nrows+1,2:ncolumns+1)=input;
factor=1/(delx^2);
clear input


% for first dimension

   output = (input2(3:nrows+2,2:ncolumns+1) - 2.*input2(2:nrows+1,2:ncolumns+1) +...
                  input2(1:nrows,2:ncolumns+1))*factor;

% for second dimension 

 output = (input2(2:nrows+1,3:ncolumns+2) - 2.*input2(2:nrows+1,2:ncolumns+1) +...
                 input2(2:nrows+1,1:ncolumns))*factor+ output;