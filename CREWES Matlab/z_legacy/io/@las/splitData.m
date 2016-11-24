function obj = splitData( obj )
%
% function obj = splitData( obj )
%
% Split data rows based on obj.delimiter using textscan
% We're using %q instead of %s to strip double-quotes from character strings
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

debug = false;

for i = 1:size(obj.sections,2)
    if obj.isDataSection(i)
        if debug, disp (['Data section: ' num2str(i)]), end
        ca = cellfun(@(X) ...
            textscan(X,'%q','delimiter',obj.delimiter,'MultipleDelimsAsOne', 1),...
            obj.sections{i}{3});
        ca = deblank(ca);
        try
            % re-order to a 2D cell-array with log data in the columns
            ca = [ca{:}]';
        catch ex
            if(debug), warning('crewes:las:splitdata',['1 ' ex.message]), end;
            % displaying exception leads to clutter...
            try
                s = repmat({max(cellfun(@length, ca))},1,length(ca));
                ca = cellfun(@(X, Y) obj.padcell(X, Y), ca, s, 'UniformOutput',false);
                
                ca = [ca{:}]';
            catch ex
               if debug, warning('crewes:las:splitdata',['2 ' ex.message]), end;
               try
                    ncols = obj.numLogs(i);
                    d=vertcat(ca{:});
                    ca=reshape(d,ncols,length(d)/ncols)';
               catch ex
                    error('crewes:las:splitdata',['3 ' ex.message]);
               end
            end
        end
        obj.sections = {'sectiondata',i,ca};
    else
        if debug, disp(['Parameter section: ' num2str(i)]), end
    end
end %end for
end %end function