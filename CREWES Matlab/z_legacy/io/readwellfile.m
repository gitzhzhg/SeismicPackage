function [X,Y,NAME] = readwellfile( filename )
% [X,Y,NAME] = readwellfile( filename )
%
% where X and Y hold coordinates (UTM) of the well positions 
% where NAME holds the names of the wells (12 char). 
% where filename is the ascii card file , where each line
%   holds the x,y,name of each well.  (This file is made using
%   readwells, a program made in the seisline environment).
%
% by T.N. Bishop, November 1993
%   see also seis2well, linewelltie
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
%filename = 'wells.dat';
[fid,message] = fopen(filename,'r');
if( fid < 0 )
	disp(' Unable to open file because:');
	disp( message );
	error(' file I/O error in readwellfile');
end
% read the next line and get the character string s 
s=fgetl(fid);
[message,errnum] = ferror(fid);
X = [];
Y = [];
NAME = [];
while   errnum == 0
  X = [X;sscanf(s,'%f',1)];
  Y = [Y;sscanf(s,'%*f %f',1)];
  nn = [sscanf(s,'%*f %*f %9s',1) zeros(1,12)];
  NAME = [NAME;nn(1:12)];
  s=fgetl(fid);
  [message,errnum] = ferror(fid);
end
message;
errnum;
disp(['number of wells = ',num2str(length(Y))]);