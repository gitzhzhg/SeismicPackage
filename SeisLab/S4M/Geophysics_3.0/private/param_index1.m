function [index,ier]=param_index1(structure,mnem,abort)
% Function outputs index for parameter info for parameter with mnemonic "mnem" 
% in structure "structure".
% If no structure field is found matching "mnem" then "index" is []
%
% Written by: E. Rietsch: September 12, 2003
% Last updated:
%
%         [index,ier]=param_index1(structure,mnem,abort)
% INPUT
% structure   structure for which parameter info is requested
% mnem   parameter mnemonic
% abort  optional parameter indicating if function should terminate abnormally
%        if no or two or more mnemonics are found
%        abort < 0 ==> interactively query for mnemonic
%        abort = 0 ==> do not terminate abnormally (default for two output arguments)
%        abort = 1 ==> terminate abnormally  (default for one output argument)
%        abort = 2 ==> do not terminate abnormally but write error message.
% OUTPUT
% index  index of row with info about the parameter with mnemonic "mnem"
% ier    error indicator; If only one output argument is given, the function will abort
%        with an error message if no parameter or more than one parameter is found
%        Otherwise: ier=0 if no error was detected and ier=1 if an error was found     

global S4M

if nargin < 3 
  if nargout == 1
    if length(dbstack) < 2
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
  index=find(ismember(structure.parameter_info(:,1),mnem));
else
  index=find(ismember(lower(structure.parameter_info(:,1)),lower(mnem)));
end

%       Check for errors
if nargout < 2 
  if isempty(index)     % Print error message
    disp([' Parameter with mnemonic "',mnem,'" not found'])
    disp(' The following parameter mnemonics exist: ')
    disp(structure.parameter_info(:,1)');
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
    disp([' More than one parameter with mnemonic "',mnem,'" found'])
    disp(' The following parameter mnemonics exist: ')
    disp(structure.parameter_info(:,1)');
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
