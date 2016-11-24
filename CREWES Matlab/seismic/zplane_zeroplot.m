function r=zplane_zeroplot(w,haxe)
% ZPLANE_ZEROPLOT plots a wavelets zeros in the Z plane
%
% r=zplane_zeroplot(w)
%
% This function uses Matlab's roots command to factor the wavelet and
% determine whether the resulting zeros are inside or outside the unit
% circle. A plot is produced showing the unit circle in the complex z plane
% and the location of every zero. The title of the plot declares whether or
% not the wavelet is minimum phase.
%
% w ... the wavelet. The First sample is assumed to be time zero
% haxe ... handle of the axis to plot in
% *********** default is to opend a new figure *************
% r ... the roots
%
% 
% by G.F. Margrave, January 2015
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

%factor the polynomial
w=w(:);%force column vector
r=roots(flipud(w));

%determine inside and outside
a=abs(r);
ind_out=a>=1;
ind_in=~ind_out;

%give purely real roots a small imaginary part for plotting
r=r+100*eps*1i;
if(nargin<2)
    figure
else
    axes(haxe);
end
hout=plot(r(ind_out),'b+');
hold
hin=plot(r(ind_in),'bo');
%plot unit circle
theta=0:360;
unitcircle=exp(1i*theta);
hunit=plot(unitcircle,'r');
axis equal
ylabel('Imaginary axis')
xlabel('Real axis')
% prepfig
if(isempty(hin))
    legend([hout hunit],'Outside','Unit circle')
    title('Signal is minimum phase')
elseif(isempty(hout))
    legend([hin hunit],'Inside','Unit circle')
    title('Signal is not minimum phase')
else
    legend([hout hin hunit],'Outside','Inside','Unit circle')
    title('Signal is not minimum phase')
end
rmax=max(abs(real(r)));
imax=max(abs(imag(r)));
rmax=max([2 1.5*rmax]);
imax=max([2 1.5*imax]);
xlim([-rmax rmax])
ylim([-imax imax])