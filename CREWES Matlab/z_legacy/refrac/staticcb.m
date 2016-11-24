function staticcb(action)
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

% Determine the parameter for the static function
if( nargin < 1 )
   action = 'init';
end
if( strcmp(action,'init'))
   q=str2mat('Enter datum elevation (m):',...
             'Use a pseudo-datum?',...
             'Use the 2nd layer velocity as replacement velocity?',...
             'Replacement velocity (m/s):',...
             'Use a constant velocity for the first layer?',...
             'Constant velocity (m/s):');
   a=str2mat('100','Yes|No','No|Yes','1600','No|Yes','600');
   askthingsinit('staticcb(''answ'')',q,a,[1 1 1 0 1 0],'Static computation');
elseif( strcmp(action,'answ'))
   a=askthingsfini;
   [strings tmp] = size(a);
   datum=str2num(a(1,:));
   repvel=str2num(a(4,:));
   if( strcmp( deblank(a(2,:)), 'Yes') )
	psd=1; % use of a pseudo-datum (method 2)
   else
	psd=0; % do not use a pseudo-datum (method 1)
   end
   if( strcmp( deblank(a(3,:)), 'Yes') )
	v2rep=1; % use of a v2 as a replacement velocity
   else
	v2rep=0; % use of a constant for replacement velocity
   end
   if(strcmp( deblank(a(5,:)),'No'))
     % Use of the first layer velocity as calculated
     v1rec = refdata('get','v1rec'); 
   else
     % Use of a specified constant first layer velocity
     v1 = str2num(a(6,:));
     [m n] = size(v2rec);
     v1rec = v1*ones(1,n);
   end
   v2rec = refdata('get','v2rec');
   depth=refdata('get','depth');
   shotcoord=refdata('get','shotcoord');
   shotelev=refdata('get','shotelev');
   recelev=refdata('get','recelev');
   uphole=refdata('get','uphole');
   % Call the static function
   [recstat,shotstat] =static(depth, shotcoord, shotelev, recelev, uphole, datum,repvel,psd, v1rec, v2rec, v2rep);
   refdata('set','recstat',recstat);
   refdata('set','shotstat',shotstat);
end