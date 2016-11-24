function indices=curve_indices(wlog,mnems,abort)
% Function outputs the column indices of curves in log structure "wlog".
% For any mnemonic in "mnem" for which there is no curve in "wlog" the 
% corresponding value of "indices" is set to zero.
% See also: curve_index1
%
% Written by: E. Rietsch: May 2000
% Last updated: November 21, 2007: rewrite; drop second output argument
%
%         indices=curve_indices(wlog,mnem)
% INPUT
% wlog   log structure whose curve(s) are requested
% mnems  curve mnemonic or cell array of curve mnemonics
% abort  optional parameter indicating if function should terminate abnormally
%        if one or more mnemonics are not found
%        abort = 0 ==> do not terminate abnormally 
%        abort = 1 ==> do not terminate abnormally, but write an error message 
%        abort = 2 ==> terminate abnormally and write error message
%        Default: abort=1
% OUTPUT
% indices  row vector of indices of the curves with mnemonics "mnems";
%        the length of "indices" is equal to the number of mnemonics "mnem". 
%        if a mnemonic has not been found the corresponding index value is
%        set to zero. If all requested mnemonics have been found then 
%        any(indices==0) == false.
%
% UPDATE HISTORY


global S4M

if nargin < 3
   abort = 1;
end

%       Check for uniqueness of the curve mnemonics
curvemnems=wlog.curve_info(:,1);
if ~S4M.case_sensitive
   curvemnems=lower(curvemnems);
   mnems=lower(mnems);
end
if length(unique(curvemnems)) ~= length(curvemnems)
   disp([' The curve mnemonics of "',wlog.name,'" are not unique.'])
   error('Abnormal termination.')
end


bool=ismember(mnems,curvemnems);

if all(bool)    % All requested mnemonics found
   [dummy,ia,ib]=intersect(curvemnems,mnems);
   indices=ia(ib);      %#ok   First output argument is not required
   return

else            % Not all mnemonics found
   if abort < 2
      indices=zeros(1,length(mnems));
      indices(bool)=find(ismember(curvemnems,mnems(bool)));
      if abort == 0
         return
      else
         display_no1(wlog,mnems,~bool)
      end
   else
      display_no1(wlog,mnems,~bool)
      error('Abnormal termination.')
   end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function display_no1(wlog,mnems,bool)

global S4M

temp=mnems(bool);
if length(temp) == 1
    disp([' Curve with mnemonic "',cell2str(temp,'", "'),'" does not exist.'])
else
    disp([' Curves with mnemonics "',cell2str(temp,'", "'),'" do not exist.'])
end

disp(' The following curve mnemonics exist: ')
if S4M.case_sensitive
   disp(['    ',cell2str(wlog.curve_info(:,1),',')]);
else
   disp(['    ',lower(cell2str(wlog.curve_info(:,1),','))]);
end