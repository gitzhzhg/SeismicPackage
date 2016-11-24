function data = GetVelocities(fig)

% data = GetVelocities(fig)
%
% Returns velocity-analysis data in a structure.
% fig .... figure number containing velocity analyis accomplished by the 
% VelocityAnalysis() function. The default is the current figure. If you
% have the wrong figure active, you'll get an empty result. 
%
% data.vrms : RMS Velocity in two-way time
% data.vave : Average velocity in two-way time
% data.vint : Interval velocity
% data.z    : Depth coordinate for interval velocity
% data.t    : Time coordinate for all velocities
%
% See also: VelocityAnalysis
%
% Chad Hogan, 2008
%
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

% $Id: GetVelocities.m,v 1.1 2009/05/25 21:00:56 cmhogan Exp $

if (nargin < 1)
    fig = gcf;
end

kids = get(fig, 'Children');
velax = kids(1);

ud = get(velax, 'UserData');

data.vrms = ud.vrmst;
data.vint = ud.vintt;
data.vave = ud.vavet;
data.z     = ud.zint;
data.t     = ud.t;