% main: input the migration parameters and apply the migration according to
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

% requirement

shotsfile='shot.segy';

velfile='vels.bin';

delfile='deltas.bin';

epsilonfile='epsilons.bin';

phifile='thetas.bin';

migfile='ttimig.bin';

dx=20;

dz=10;

nxo=456;

nz=200;

nxshot=1;

lpad=20;

rpad=20;

fpeak=25;

ixshot_start=50;

%tti migration
premigrtmtti(shotsfile, velfile, epsilonfile, delfile, phifile, ...
             dx, dz, nxo, nz, nxshot,lpad, rpad, fpeak, ixshot_start, migfile);

plotfile(migfile, nxo, nz, dx, dz);