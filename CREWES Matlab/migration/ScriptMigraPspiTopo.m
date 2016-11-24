
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

%  Nov-2011

% Script Migra Pspi from topography - Converted wave
% Method shot profile
% An example

% Requirements:
%   * The velocity models for P and S waves
%   * The seismic data of the horizontal component. Better if data Zero 
%       Phase to obtain the correct depths.
%   * Aditional data required: field record, its coordinates (t,x),
%       velocity model of the source (velp), velocity model of the 
%       receiver (vels), coordinates of these models (xv, xv), coordinates
%       of the energy source (xshot, zshot), surface topography (zt)
%       
% It can also be used for pure PP reflections, using the seismic data of
% the vertical component as an input, and the P-wave time tables for the
% receivers.
%


%% Migration PSPI - Three shots


[smdc41x,smcc41x,illum41x]=pspi_shot_cwavez(topo41xs,t,x,velp,vels,xv,zv,xshot1,zshot1,zt,[0 60],0.001);
[smdc45x,smcc45x,illum45x]=pspi_shot_cwavez(topo45xs,t,x,velp,vels,xv,zv,xshot5,zshot5,zt,[0 60],0.001);
[smdc49x,smcc49x,illum49x]=pspi_shot_cwavez(topo49xs,t,x,velp,vels,xv,zv,xshot9,zshot9,zt,[0 60],0.001);

save migraPspicTopoResult
%% Migration Stack
% stkmigcc=smcc41x+smcc45x+smcc49x;
% plotimage(stkmigcc,zv,xv)


%%