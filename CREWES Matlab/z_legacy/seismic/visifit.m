function [p,coeff]= visifit(y,x,norder)
% [p,coeff]= visifit(y,x,norder)
%
% VISIFIT allows the interactive fitting of an n'th order 
% polynomial to the data y(x). After plotting y(x) VISIFIT 
% graphical input mode and waits for the user to click both
% ends of the x coordinate range to be used in the fit.
%
% G.F. Margrave
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
x=x(:)';
y=y(:)';
% plot y(x)
 plot(x,y,'.')
 grid;
 text(.1,.05,'Select x coordinate range','sc');
% get the domain of the fit
[x1,y1]=ginput;
 while ~isempty(x1)
  xmin=min(x1);
  xmax=max(x1);
  if xmin==xmax, error(' Zero length domain chosen'),end
% fit the polynomial
  infit=near(x,xmin,xmax);
  coeff=polyfit(x(infit),y(infit),norder);
  p=polyval(coeff,x);
% draw new graph
   ind = find( p>1.1*max(y) );
   p(ind) = ones(size(p(ind)))*max(y);
   ind2 = find( p<min(y) );
   p(ind2) = ones(size(p(ind2)))*min(y);
  %plot(x,[y;p])
   plot(x,y,'.',x,p,'m')
   hold
   plot(x(ind),p(ind),'*m');
   plot(x(ind2),p(ind2),'*m');
  grid;
  text(.1,.00,'Select x coordinate range','sc');
  [x1,y1]=ginput;
end