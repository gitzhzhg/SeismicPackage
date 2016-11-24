function [spec, kx, ky] = fftgrid(grin, x,y, padflag, pctaper, meanflag)
% [spec, kx, ky] = fftgrid(grin, x,y, padflag, pctaper, meanflag)
% [spec, kx, ky] = fftgrid(grin, x,y, padflag, pctaper)
% [spec, kx, ky] = fftgrid(grin, x,y, padflag)
% [spec, kx, ky] = fftgrid(grin, x,y) 
%  
% fftgrid computes the 2-D fft of a real valued grid. The returned spectrum has
% been run through fftshift to place the origin at the center. kx and ky are
% vectors containing the wavenumbers for the columns and rows of spec. fftgrid
% will automatically remove the mean from the input grid.
%
% grin = input grid (matrix)
% x = vector of coordinates labeling the columns of grin
%    !!! length(x) must equal the number of columns of grin !!!
% y = vector of coordinates labeling the rows of grin
%    !!! length(x) must equal the number of rows of grin !!!
% padflag = 0 ... transform grin as input without padding
%           1 ... pad the rows and columns of grin with zeros to
%                 the next larger power of 2 in size.
%   ********* default = 1 **************
% pctaper = length in percent of a raised cosine taper to apply
%           around the boundaries of grin prior to transforming
%  *********** default = 20 ************
% meanflag = 0 ... transform grin as input without removing the mean
%           	1 ... subtract the mean value from grin prior to the
%                  transform
%   ********* default = 1 **************
%
% spec = complex spectrum of the possibly padded and tapered input
%        grid
% kx = row vector giving the wavenumber coordinates of the columns
%      of spec
% ky = column vector giving the wavenumber coordinates of the rows 
%      of spec 
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
if(nargin < 6) meanflag = 1; end
if( nargin < 5) pctaper = 20; end
if( nargin < 4) padflag = 1; end
[rowin colin]=size(grin);
% remove the mean from the input grid
if( meanflag)
	grin = grin - mean(mean(grin));
end
% apply the taper
if( pctaper > eps )
   rowtaper = mwindow(colin, pctaper);
   rowtaper = ones(rowin,1)*rowtaper;
   coltaper = mwindow(rowin,pctaper)';
   coltaper = coltaper*ones(1,colin);
   grin = grin.*coltaper.*rowtaper;
end
	coltaper=[];
	rowtaper=[];
   
% pad to a power of 2
if( padflag)
		rowpad = pow2(nextpow2(rowin));
		colpad = pow2(nextpow2(colin));
		grin = gridpad( grin, rowpad, colpad);
end
% extend x and y
  x = xcoord(x(1),x(2)-x(1),colpad);
  y = xcoord(y(1),y(2)-y(1),rowpad);
		
% compute the fft and shift it
		spec = fftshift(fft2(grin));
		
% compute the wavenumber vectors
		kxnyq = 1./(2.*(x(2)-x(1)));
		kynyq = 1./(2.*(y(2)-y(1)));
		dkx = 2.*kxnyq/length(x);
		dky = 2.*kynyq/length(y);
		kx = -kxnyq:dkx:kxnyq-dkx;
		ky = -kynyq:dky:kynyq-dky;