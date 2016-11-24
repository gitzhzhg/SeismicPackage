function PI_zoomoptions
%
% function PI_zoomoptions
%
% Sets and Unsets global ZOOM_VALUE and ZOOM_LOCKS
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

global ZOOM_VALUE ZOOM_LOCKS

% vall is a cell array wehre
%   gcbo   = handle of object which called PI_zoomoptions
%   val{1} = handle of the figure we wish to synchronize our zoom to
%   val{2} = action to take (publish || match || lock || unlock)
val=get(gcbo,'userdata');

switch val{2};   
    case 'publish'
        % Publish current axes zoom values
        %    gca = handle to current axis of current figure
        ZOOM_VALUE={sort(get(gca,'xlim')) sort(get(gca,'ylim'))};
    case 'match'
        % Set current axes to published zoom values
        %    gca = handle to current axis of current figure        
        if(~isempty(ZOOM_VALUE))
            set(gca,'xlim',ZOOM_VALUE{1},'ylim',ZOOM_VALUE{2});
        end
    case 'lock'
        % zoom lock current plot to another plot
        %      gcf = handle to current figure
        %      gca = handle to current axis of current figure  
        %   val{1} = handle of the target figure we are locking to
        
        % get handle to mainax of target figure
        hmainax=findobj(val{1},'type','axes','tag','MAINAXES');
        
        % set current axes of current figure to match mainax of target
        % figure
        set(gca,'xlim',get(hmainax,'xlim'),'ylim',get(hmainax,'ylim'));
        
        % Add new lock to ZOOM_LOCKS
        ZOOM_LOCKS=[ZOOM_LOCKS; gcf val{1}];
    case 'unlock'
        % Remove current figure from ZOOM_LOCKS
        ZOOM_LOCKS(ZOOM_LOCKS(:,1) == gcf,:)=[];
    otherwise
        warning('crewes:displaytools:plotimage:zoomoptions','unknown option');
end %end switch val{2}

end %end function PI_zoomoptions