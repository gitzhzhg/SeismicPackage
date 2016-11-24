function [pressure_out]=ders_5pt(pressure,density,delx)
% DERS2_5PT ... compute the 5 point divergence (1/density * grad pressure)
%
% [pressure_out]=ders2_5pt(pressure,density,delx)
%
% DERS2_5PT computes the 5 point approximation of the spatial derivative 
% the two dimensional matrices 'input' and 'density.  The horizontal and
% vertical bin spacing of both matrices MUST be the same and equal.    
%
% pressure = input pressure matrix
% density = input density matrix
% delx = the horizontal/ vertical bin spacing in consistent units
%
% pressure_out = output pressure matrix
%
% by Carris Youzwishen, April 1999
% extended to full acoustic wave equation by Hugh Geiger, September 2003 
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

[nrows,ncolumns]=size(pressure);

pres=zeros(nrows+2,ncolumns+2);
pres(2:nrows+1,2:ncolumns+1)=pressure;
pres(1,:)=0;
pres(end,:)=pres(end-1,:);
pres(:,1)=pres(:,2);
pres(:,end)=pres(:,end-1);
dens=zeros(nrows+2,ncolumns+2);
dens(2:nrows+1,2:ncolumns+1)=density;
dens(1,:)=dens(2,:);
dens(end,:)=dens(end-1,:);
dens(:,1)=dens(:,2);
dens(:,end)=dens(:,end-1);
factor=1/(2*delx^2);
clear pressure
clear density


% for first dimension

   pressure_out = (pres(3:nrows+2,2:ncolumns+1) - pres(2:nrows+1,2:ncolumns+1)).*...
                  (dens(3:nrows+2,2:ncolumns+1) + dens(2:nrows+1,2:ncolumns+1))./...
                   dens(3:nrows+2,2:ncolumns+1)*factor-...
                  (pres(2:nrows+1,2:ncolumns+1) - pres(1:nrows,2:ncolumns+1)).*...
                  (dens(2:nrows+1,2:ncolumns+1) + dens(1:nrows,2:ncolumns+1))./...
                   dens(1:nrows,2:ncolumns+1)*factor;

% for second dimension 

 pressure_out = (pres(2:nrows+1,3:ncolumns+2) - pres(2:nrows+1,2:ncolumns+1)).*...
                (dens(2:nrows+1,3:ncolumns+2) + dens(2:nrows+1,2:ncolumns+1))./...
                 dens(2:nrows+1,3:ncolumns+2)*factor-...
                (pres(2:nrows+1,2:ncolumns+1) - pres(2:nrows+1,1:ncolumns)).*...
                (dens(2:nrows+1,2:ncolumns+1) + dens(2:nrows+1,1:ncolumns))./...
                 dens(2:nrows+1,1:ncolumns)*factor+pressure_out;