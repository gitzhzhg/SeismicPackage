function shot = shotrange(string)
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

[s1, rest] = strtok(string,',');
[s2, rest] = strtok(rest,',');
a = sscanf(s1, '(%d:%d)');
if(isempty(a))
   a = sscanf(s1, '%d');
   i1 = a;
   i2 = a;
else
   i1 = a(1);
   i2 = a(2);
end
b = sscanf(s2, '(%d:%d)');
if(isempty(b))
   b = sscanf(s2, '%d');
   j1 = b;
   j2 = b;
else
   j1 = b(1);
   j2 = b(2);
end
% Now, 'a' contains the range of the first shots (either [1] or [1 2]),
% and 'b' contains the range of the 2nd shots.
% Generate a list of shot pairs
k=0;
for i=i1:i2
   for j=j1:j2
      if( i<j )
         k = k+1;
         shot(k,1) = i;
         shot(k,2) = j;
      end
   end
end