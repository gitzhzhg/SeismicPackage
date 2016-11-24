function PI_MovePickLineStart()
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

nm=get(gco,'tag');
switch nm
    case 'PICKMARKER'
        set(gcf,'name','Click & Hold MB1 on Markers to move.  MB3 menu, or function change to stop line moving')
        set(gcf,'windowbuttonmotionfcn','plotimage(''MovePickLineMotion'')',...
            'windowbuttonupfcn','plotimage(''MovePickLineEnd'')');
       % %  udat=get(gco,'userdata');
        
       % set(gco,'erasemode','xor');
       % set(findobj(gcf,'type','line','tag','PICKS'),'erasemode','xor');
    case 'PICKS'
        sltype=get(gcf,'selectiontype');
        switch sltype
            case 'alt'
                % will continue on and open line menu
            case 'normal'
                udat=get(gco,'userdata');
                if(isempty(udat))
                elseif(~ishandle(udat(1)))
                else
                    cpt=get(gca,'currentpoint');
                    set(gca,'userdata',cpt);
                  %  set(gco,'erasemode','xor');
                  %  set(udat(1),'erasemode','xor');
                  %  set(udat(2),'erasemode','xor');
                    set(gcf,'name','Click & Hold MB1 on Markers to move.  MB3 menu, or function change to stop line moving')
                    set(gcf,'windowbuttonmotionfcn','plotimage(''MovePickLineMotion'')',...
                        'windowbuttonupfcn','plotimage(''MovePickLineEnd'')');
%                     set(findobj(gcf,'type','line','tag','PICKS'),'erasemode','xor');
                end
            case 'More To Come'
        end    
end