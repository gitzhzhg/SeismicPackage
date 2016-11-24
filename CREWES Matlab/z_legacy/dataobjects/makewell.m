function well=makewell(name,location,wellog)
%
% well=makewell(name,location,wellog)
%
% Make a earthobject representing a well with name, location, and 
% (optionally) a log
%
% name = string giving the well name
% location = 3 or 4 element vector giving: [x y elevation inline_distance]
% wellog = random Earth Object containing the log. If the log is in depth 
%          then it should have two data fields: 'depth' & 'samples' and 
%          be of datatype 'zlog', else it should have data fields 
%          'time' and 'samples' and be of type 'tlog'
%
% by G.F. Margrave
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

% Make an empty well object
well=contobj(name,'well');

% put the location information in
well=objset(well,'location',location);

% put the log in
if( nargin > 2)
	nam=objget(wellog,'name');
	well=objset(well,nam(:)',wellog);
end