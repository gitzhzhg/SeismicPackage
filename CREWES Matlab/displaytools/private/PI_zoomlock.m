function PI_zoomlock()
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

global ZOOM_LOCKS
mainax=findobj(gcf,'type','axes','tag','MAINAXES');
% posax=findobj(gcf,'type','axes','tag','POSITIONAXES');
hgcf=gcf;
if(~isempty(ZOOM_LOCKS))
    hgca=mainax;
    ydat=get(hgca,'ylim');
    xdat=get(hgca,'xlim');
    for ii=1:size(ZOOM_LOCKS,1)
        CheckLock=ZOOM_LOCKS(ii,:);
        if(CheckLock(2)==hgcf)
            SlaveAxes=findobj(CheckLock(1),'type','axes','tag','MAINAXES');
%             SlaveAxesChildren=get(SlaveAxes,'children');
            set(SlaveAxes,'ylim',ydat,'xlim',xdat);
            hslave=get(SlaveAxes,'Parent');
            set(0,'currentfigure',hslave);
            hslavedat=get(hslave,'userdata');
            hvertscrol=hslavedat{16};
            hhorscrol=hslavedat{17};
            % sliders on other figures are being shut off to avoid
            % problems with sliders being set wrong
            set(hhorscrol,'visible','off');
            set(hvertscrol,'visible','off');
            PI_positionaxes_lineposition;
        end
    end
end
set(0,'currentfigure',hgcf);