function namecolorinit(transfer,q1,v1,q2,v2,q3,namelist)
% Please use "namecolorle" when developing new programs.
%
% namecolorinit(transfer,q1,v1,q2,v2,q3,namelist)
% namecolorinit(transfer,q1,v1,q2,v2)
%
% Initiate a name/color selection dialog.
% transfer = a string matlab command to be evaluated with EVAL at the 
%            completion of the dialog. Usually this will re-invoke the 
%            calling program
%	q1 = string giving the question to be asked to provide the name
%	v1 = the default value of the name. May be ''
%	q2 = string giving the question asking for a color.
%       v2 = default value of the color as an rgb triplet. [.5 .5 .5] 
%            gives a neutral grey
%       q3 = optional third question associated with the namelist
% namelist = optional vector of names. If present, a toggle button will 
%            be present allowing the current object to be named the same 
%            as a previous one. If clicked, the namefield and color 
%            selection will disappear to be replaced by a popup menu
%            allowing the selection of one of the names in the vector. 
%            Namelist should be in the format returned by 
%            objget(object,'fieldnames') 
%
% G.F. Margrave November 1993
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
if( nargin == 6 )
   error('incorrect number of arguments');
end
if(nargin<6)
   q3=[];
   namelist=[];
end
if(nargin<5)
   error('minimum of 5 arguments required');
end
[m,n]=size(namelist);
if( m > 1 )
   error(' namelist must be a vector ');
end
l=max([length(q1) length(q2) length(v1) length(transfer) length(v2) ...
	length(q3) n]);
if( m == 0)
   qs=ones(5,l);
else
   qs=ones(7,l);
end
qs(1,1:length(q1))=q1;
qs(2,1:length(v1))=v1;
qs(3,1:length(q2))=q2;
qs(4,1:length(transfer))=transfer;
qs(5,1:length(v2))=v2;
if( m>0 )
   qs(6,(1:length(q3))) = q3;
   qs(7,1:n)=namelist;
end
set(gca,'userdata',qs);
namecolor('init');