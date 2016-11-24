function PI_delete_amp_picks(hfig)
%
% deletes the AMP_PICKS for the figure hfig
% Gets called either by PI_Close or PI_zoompick
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

%

global AMP_PICKS
nevents=length(AMP_PICKS);
knew=0;
new_amp_picks=cell(1,nevents);
for k=1:nevents
    pickstruc=AMP_PICKS{k};
    if(pickstruc.figurehandle~=hfig)
        knew=knew+1;
        new_amp_picks{knew}=AMP_PICKS{k};
    else
        %yes this really is necessary
        if(ishandle(pickstruc.handle))
            delete(pickstruc.handle);
        end
        if(ishandle(pickstruc.trajhandle))
            delete(pickstruc.trajhandle);
        end
        if(ishandle(pickstruc.texthandle))
            delete(pickstruc.texthandle);
        end
    end
end
AMP_PICKS=new_amp_picks(1:knew);