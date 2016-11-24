function c = getTopsSectionsAsChar(obj,tp,tdef,tdat,outputversion)
%
% function c = getTopsSectionsAsChar(obj,tp,tdef,tdat,outputversion)
%
%   Return formated char from cell array appropriate for a given LAS
%   version
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
    c=[];
            
    if inputversion < 3.0 && outputversion >= 3.0
        disp ('convert from v2 to v3')
    %convert from version 2 to version 3               
        sectionhead = { ...
%           '~tp                                    '; ...  
%           'TOPS.    Prognosis    : Formation Source        {S}'; ...
%           'TOPDR.   Subsea       : Tops Depth Reference    {S}'; ...
          '~Tops_tdef                                   '; ... 
          'TOPT.M                : Formation Top Depth     {F}'; ...
%           'TOPB.M                : Formation Base Depth    {F}'; ...
          'TOPN.                 : Formation Top Name      {S}'; ...            
          '~tdat | Tops_tdef                       '; ...  
        };

        %create LAS3 tops data
        d(:,2) = obj.getSectionMnemonics(tp)';
        d(:,1) = obj.getSectionValues(tp)';
        data   = obj.joinDataSectionData(d,obj.delimiter);
   
        %combine header and data
        c = char(vertcat(sectionhead,data));
                
    elseif inputversion >= 3.0 && outputversion < 3.0
        % convert version 2 tops to version 3 tops
         
      % tops_prm = obj.getSection(tp)
      % tops_def = obj.getSection(tops_tdef)
      tops_dat = obj.getSection(tdat);
      
      if ~isempty(tops_dat)
          [topnames, topdepths] = obj.getTops(tp,tdef,tdat);
          topnames = cellstr(topnames);            %convert char array to cell array
          topnames = regexprep(topnames,'\s+','_');%replace spaces with underscores
          
          tpar{1} = cellstr('~Tops');
          tpar{2} = cell(1,0);
          
          tpar{3} = repmat(cellstr(blanks(1)),6,length(topnames));          
          tpar{3}(1,:) = topnames;
          tpar{3}(3,:) = cellstr(num2str(topdepths,'%.4f'));
          
          tpar{4}      = false;
          c = obj.getSectionAsChar(tpar);
      else
          c = [];
          
      end
    else          
           ca = { obj.getSectionAsChar(tp); ...
                  obj.getSectionAsChar(tdef); ...
                  obj.getSectionAsChar(tdat,' '); ...
           };
           
           %remove empty lines
           ca(cellfun(@isempty,ca)) = [];
           c=char(ca);      
      
    end
        
end %end function