function c = convm(a, b)
% c= convm(a,b)
%
% convm is a modification of the 'conv' routine from the MATLAB
% toolbox. The changes make it more convenient for seismic purposes
% in that the output vector, c, has a length equal to the first
% input vector,  a. Thus, 'a' might correspond to a reflectivity
% estimate to be convolved with a wavelet contained in 'b' to
% produce a synthetic seismic response 'c'. It is assumed that
% the wavelet in b is causal and that the first sample occurs at time zero.
% For non-causal wavelets, use 'convz'. An abort will occur if
% b is longer than a. CONVM will correctly handle an ensemble matrix.
% 
%CONV	Convolution and polynomial multiplication.
%	C = CONV(A, B) convolves vectors A and B.  The resulting
%	vector is length LENGTH(A)
%	If A and B are vectors of polynomial coefficients, convolving
%	them is equivalent to multiplying the two polynomials.
%	See also XCORR and DECONV and CONVZ
%
%	J.N. Little 4-21-85
%	Revised 9-3-87 JNL
%	Copyright (c) 1985, 1987 by the MathWorks, Inc.
%
% modified to 'convm' by G.F. Margrave, May 1991
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
[rows,cols]=size(a);  
if rows>cols, a=a.';end
na = max(size(a));
nvecs= min(size(a));
nb = max(size(b));
% Convolution, polynomial multiplication, and FIR digital
% filtering are all the same operations.  Since FILTER
% is a fast built-in primitive, we'll use it for CONV.
% This routine assumes that the first argument is the 
% which is longer than the wavelet (second argument). An
% abort occurs if this is not so.
%
if na<nb, error(' First vector must be longer than second'),end
if nvecs==1,
 
	  if na > 1
		  a(na) = 0;
	  end 
  	c = filter(b, 1, a);
   mw=mwhalf(na,4);
   c=c.*mw;
 else
  c=zeros(size(a));
  mw=mwhalf(na,4); 
  for k=1:nvecs
    c(k,:)=filter(b,1,a(k,:)).*mw;
  end
 end
if rows>cols, c=c.';end