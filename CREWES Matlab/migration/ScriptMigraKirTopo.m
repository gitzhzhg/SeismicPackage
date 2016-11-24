%  Nov-2011
%
% Script Migra Kirchhoff from topography - Converted wave
% Method shot profile
%
% An example
%
% Requirements:
%   * The time tables for Sources and Receivers
%   * The seismic data of the horizontal component. Better if data Zero 
%       Phase to obtain the correct depths.
%   * Aditional data required: field record, its coordinates (t,x),
%       velocity model of the source, velocity model of the 
%       receiver, coordinates of these models (xv, xv), coordinates
%       of the energy source (xshot, zshot), surface topography (zt)
%       
%
% It can also be used for pure PP reflections, using the seismic data of
% the vertical component as an input, and the P-wave time tables for the
% receivers.
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

%% Migration Kirchhoff - Three shots


[smigt41x,smigz41x,tmig41x,xmig41x,vmod41x]=kirk_shotcz(topo41xs,t,x,zt,xshot1,vp2,vs2,xv,zv,tr4s19(:,:,1),tr4rall,[]);
[smigt45x,smigz45x,tmig45x,xmig45x,vmod45x]=kirk_shotcz(topo45xs,t,x,zt,xshot5,vp2,vs2,xv,zv,tr4s19(:,:,5),tr4rall,[]);
[smigt49x,smigz49x,tmig49x,xmig49x,vmod49x]=kirk_shotcz(topo49xs,t,x,zt,xshot9,vp2,vs2,xv,zv,tr4s19(:,:,9),tr4rall,[]);

save migraKirkcTopoResult

%% Stack Migra
stkMig19=smigz41x+smigz45x+smigz49x;
plotimage(stkMig19,zv,xmig45x)
title('Stack C-wave Mig Kir 1,5,9 ')
grid