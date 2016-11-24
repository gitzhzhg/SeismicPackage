function functionName=name_of_calling_function(ik)
% Function determines the name of the function in the calling sequence that
% lead to this call
%
% Written by: E. Rietsch: October 29, 2005
% Last updated:
%
%         index1=find(ismember([11,22],[3,4,22,11]))
% INPUT
% ik      index specifying the level up 
%         1 function that called "name_of_calling_function",
%         2 function that called the function that called "name_of_calling_function",
%           ....
% OUTPUT
% functionName  name of the requested function


try
   temp=dbstack;
   functionName=temp(ik).name;
   
catch
   functionName='???';
end
