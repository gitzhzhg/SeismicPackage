function [index,ier]=curve_index1(wlog,mnem,abort)
% Function outputs index of curve with mnemonic "mnem" in log structure "wlog".
% If no curve mnemonic is found matching "mnem" and if the function has been
% requested not to abort then "index" is set to [].
%
%         [index,ier]=curve_index1(wlog,mnem,abort)
% INPUT
% wlog   log structure whose curve are requested
% mnem   curve mnemonic
% abort  optional parameter indicating if function should terminate abnormally
%        if no or two or more than mnemonics are found
%        abort < 0 ==> interactively query for the mnemonic
%        abort = 0 ==> do not terminate abnormally (default for two output arguments)
%        abort = 1 ==> terminate abnormally  (default for one output argument)
%        abort = 2 ==> do not terminate abnormally but write error message.
% OUTPUT
% index  index of curve with mnemonic "mnem"
% ier    error indicator; If only one output argument is given, the function will abort
%        with an error message if no curve or more than one curve is found
%        Otherwise: ier=0 if no error was detected and ier=1 if an error was found     

global S4M

if nargin < 3 
   if nargout == 1
      if S4M.deployed
         abort=-1;
      else
         abort=1;
      end
   else
      abort=0;
   end
end

index=[];

                  while isempty(index)

if S4M.case_sensitive
   index=find(ismember(wlog.curve_info(:,1),mnem));
else
   index=find(ismember(lower(wlog.curve_info(:,1)),lower(mnem)));
end

%       Check for errors
if nargout < 2 
   if isempty(index)     % Print error message
      if abort ~= 0
         disp([' Curve with mnemonic "',mnem,'" not found.'])
         disp(' The following curve mnemonics exist: ')
         disps(cell2str(wlog.curve_info(:,1),', '))
      end
      if abort < 0
         mnem=input(' Enter correct mnemonic (must be enclosed by quotes {''}: ');
         if isempty(mnem)
            error(' Abnormal termination')
         end

      elseif abort == 1   
         error(' Abnormal termination')
      else
         return
      end

   elseif length(index) > 1     % Print error message
      disp([' More than one curve with mnemonic "',mnem,'" found.'])
      disp(' The following curve mnemonics exist: ')
      disps(cell2str(wlog.curve_info(:,1),', '))
      if abort == 1
         error(' Abnormal termination')
      else
        return
      end

   else
      return
   end

else
   if isempty(index) || length(index) > 1
      ier=1;
   else
      ier=0;
   end
   break
end

                  end    % end "while' loop
