function vspflat=VSP_flatten(vsp,t,tp,dir,flag)
% VSP_flatten: flatten an event on a vsp by static shifts
% 
% vspflat=VSP_flatten(vsp,t,tp,dir,flag)
%
% vsp ... input vsp matrix. One trace per column
% t   ... time coordinate for vsp
% tp  ... vector of times for the event to be flattened. There must be one
%       value per trace.
% Requirement: length(t) must equal size(vsp,1) AND length(tp) must equal
%               size(vsp,2).
% 
% dir ... 1 means to flatten 
%         -1 means to unflatten
% flag ... 1 means to apply static shifts as specified by tp-tp(1)
%          2 means to apply the static tp-mean(tp). 
% Note: Flag=2 will cause the event to be flattened at its mean time while
%     flag=1 causes the event to be flattened at the time of the first
%     receiver.
%
% vspflat ... output flattened (or unflattened) vsp
%
%
% G.F. Margrave, 2014, CREWES
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

if(flag==1)
    dtau=-tp+tp(1);%% this will flatten it at the time of the first receiver
elseif(flag==2)
    dtau=-tp+mean(tp);%% this will flatten it at the mean time
elseif(flag==3)
    dtau=-tp+tp(end);
else
    error('invalid value for flag')
end

if(abs(dir)~=1)
    error('invalid value for dir');
end
if(length(t)~=size(vsp,1))
    error('t and vsp are not size compatible')
end
if(length(tp)~=size(vsp,2))
    error('tp and vsp are not size compatible')
end

vspflat=stat(vsp,t,dir*dtau);