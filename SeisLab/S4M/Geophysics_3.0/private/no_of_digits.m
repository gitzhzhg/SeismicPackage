function nDigits=no_of_digits(num,maxDigits)
% Compute number of digits to represent a number for display
%
% Written by: E. Rietsch: September 27, 2006
% Last updated: January 20, 2007
%
%         nDigits=no_of_digits(num,maxDigits)
% INPUT    
% num   number 
% OUTPUT   
% nDigits  number of digits used to represent "num" (3<= nDigits <= maxDigits)

nDigits=min(abs(floor(log10(abs(num))))+1,maxDigits);
nDigits=max(3,nDigits);
