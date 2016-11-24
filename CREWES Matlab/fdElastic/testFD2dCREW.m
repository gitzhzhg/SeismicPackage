%testFD2d - Run 2D finite-difference model
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

clear
global tictoc
tictoc = clock;
 tic
 
% F2dmats6                           %Latest
% F2dmats7                           %Latest

% load('c3p20s10m50.mat')
% whos

 surfB5FD2d    %compB4AllFD2d     Getr4F resampSFplot %Latest

% [generic] = fdReadgeo2('zone.geo');

% [Dt,Dxz,fractx,nWide,iPlTerm,wncvar,pvel,svel] = ....
%            readParmsCr4('test.parc');

%disp(Uz(1:3,1:3))
%disp(Ux(1:3,1:3))
% figure
% plot(surfUz(2,:))
% grid on
% title('Corrected, ixm2 = 2:ix9')
% print -dtiff -r150 corr2.tif
% xA = 1:400;
% plot(xA,wellUx(140,:),xA,wellUx(30,:))
% grid on
%help deconw
%help seismic
%help ormsby
%help resamp
%help sinci
%help plotseis
%help displaytools
%help plotimage
% figure
% whitefig
% print -dtiff -r150 whiteF.tif
%whos
%help fftrl
 toc