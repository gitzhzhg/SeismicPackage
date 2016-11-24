function y = fftshiftm(x)
% y= fftshiftm(x)
%
% This corrects the version of FFTSHIFT supplied in the Matlab toolbox
% which is erroneous for matricies. For an input matrix [1 2;3 4],
% FFTSHIFT returns [4 3;1 2] while FFTSHIFTM returns [3 4;1 2]
%
% Correction by G.F. Margrave, May 1991
%
%FFTSHIFTM Shift FFT.  For vectors FFTSHIFT(X) returns a vector with the
%	left and right halves swapped.  For matrices, FFTSHIFT(X) swaps
%	the first and third quadrants and the second and fourth quadrants.
%	FFTSHIFT is useful for FFT processing, moving the zeroth lag to
%	the center of the spectrum.
%
%	J.N. Little 6-23-86
%	Copyright (c) 1986 by the MathWorks, Inc.
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
[m,n] = size(x);
m1 = 1:floor(m/2);
m2 = floor(m/2)+1:m;
% Note: n/2+1 references the Nyquist. so the spectrum after FFTSHIFT 
% goes from -Nyquist -> DC -> one sample before + Nyquist
n1 = 1:floor(n/2);
n2 = floor(n/2+1):n;
% Note: can remove the first two cases when null handling is fixed.
if m == 1
	y = [x(n2) x(n1)];
elseif n == 1
	y = [x(m2); x(m1)];
else
 y = [x(m2,n1) x(m2,n2); x(m1,n1) x(m1,n2)];
end