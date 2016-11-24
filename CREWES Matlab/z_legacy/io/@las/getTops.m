function [tn, td] = getTops(obj,tparm,tdef,tdat)
%
% function [tn, td] = getTops(obj,tp,tdef,tdat)
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

inputversion = str2double(obj.version);

%assume failure
tn='';
td=[];

%inputversion < 3
if inputversion < 3.0
    tops_parameter = tparm;
    tops_dat = obj.getSectionData(tops_parameter);
    if isempty(tops_dat)
        return
    end
    tn = char(tops_dat(1,:));
    td = str2double(tops_dat(3,:))';
elseif inputversion >= 3.0
    %         tops_parameter  = '~tops_parameter';
    %         tops_definition = '~tops_definition';
    %         tops_data       = '~tops_data';
    
    tops_dat = obj.getSection(tdat);
    if isempty(tops_dat)
        return
    end
    %get tops names
    tn  = char(tops_dat{3}(:,...
        obj.getMnemonicIndex(tdef,'topn')));
    %get tops
    td  = str2double(tops_dat{3}(:,...
        obj.getMnemonicIndex(tdef,'topt')));
    
    %convert tops elevations to depths
    if strncmpi(obj.getMnemonicValue(tparm,'topdr'),...
            'Subsea',6)
        %get reference elevation
        % THIS needs work; check DREF for starters...
        eref = obj.getMnemonicValue('~parameter','eref');
        
        if isempty(eref)
            eref = obj.getMnemonicValue('~log_parameter','eref');
        end
        
        %convert subsea to depth
        eref = str2double(eref);
        
        if ~isnan(eref)
            td = eref -td;
        end
    end
    
    
    
end
        
end %end function