function mnem=mnem4eimp(method,angleDeg)
% Create a string with the mnemonic of the elastic impedance computed by
% mewthod "method" for angle "angleDeg.
%
% Written by: E. Rietsch: November 30, 2006
% Last updated:
%
%         mnem=mnem4eimp(method,angleDeg);
% INPUT
% method  string with one of the valis methods to compute elastic impedance.
%         Presently, possible strings are: 'Aki','Bortfeld','Shuey',
%                                          'Hilterman','Rueger','two-term'
% angleDeg  angle of incidence in degrees
% OUTPUT
% eimp_mnem  string with mnemonic for the elastic impedance
%          e.g. eimp60b   (elastic impedance for 60 degrees computed with 
%	                  Bortfeld's method)
%
%
% EXAMPLE
%         mnem=mnem4eimp('Aki',40)

if length(angleDeg) ~= 1
   error(' The angle must be a conststant.')
end

mnem=['eimp',num2str(angleDeg),lower(method(1))];
