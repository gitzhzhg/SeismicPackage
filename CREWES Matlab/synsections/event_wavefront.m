function amat=event_wavefront(amat,t,x,tnot,xnot,v,amp,aper)
% EVENT_WAVEFRONT: inserts a wavefront (circle) event in a matrix.
%
% amat=event_wavefront(amat,t,x,tnot,xnot,v,amp,flag,aper)
%
% EVENT_WAVEFRONT inserts a wavefront circle event in a matrix. This is
% done in the time domain so that the reciprocal nature of diffraction
% hyperbolae and wavefront circles can be easily demonstrated.
%
% amat ... the matrix of size nrows by ncols
% t ... vector of length nrows giving the matrix t coordinates
% x ... vector of length ncols giving the matrix x coordinates
% tnot ... t coordinate of the wavefront nadir (lowest point)
% xnot ... x coordinate of the wavefront nadir (lowest point)
% v ... velocity of wavefront. Value is divided by two to simulate post
%       stack.
% amp ... amplitude of the wavefront
% aper ... truncate wavefront beyond this aperture
% ******** default is no truncation ********
%
% G.F. Margrave, CREWES Project, University of Calgary, 2014
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

if( nargin <8 )
	aper = inf;
end

v=v/2;
r=v*tnot;%radius of the circle
r=min([r aper]);
x1=xnot-r;
x2=xnot+r;
ix=near(x,x1,x2);%these are the columns of interest

%loop over columns
[nsamp,nc]=size(amat);

dt=t(2)-t(1);
tmin=t(1);

for k=ix
    xoff=x(k)-xnot;
    tk = sqrt(r^2-xoff^2)/v;
    a=amp;
    ik=(tk-tmin)/dt+1;
    if( between(1,nsamp,ik) )
        ik1=floor(ik);
        ik2=ceil(ik);
        if(ik1==ik2)
            %exactly on a sample
            amat(ik1,k)=amat(ik1,k)+a;
        else
            %a simple interpolation
            amat(ik1,k)=amat(ik1,k)+a*(ik-ik2)/(ik1-ik2);
            amat(ik2,k)=amat(ik2,k)+a*(ik-ik1)/(ik2-ik1);
        end
    end
end