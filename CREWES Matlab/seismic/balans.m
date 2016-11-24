function trout=balans(trin,trref,tz)
% BALANS: match the rms power of one trace to another
%
% trout=balans(trin,trref,tz)
% trout=balans(trin,trref)
% trout=balans(trin)
%
% BALANS adjusts the rms power of trin to equal that of trref
%
% trin= input trace or gather to be balanced
% trref= input reference trace
% ******** default= ones(trin) *******
% tz = vector of indicies specifying a time zone over which balancing is to
%     be done.
% trout= balanced output trace
%
% NOTE: If trin is a matrix, then traces are assumed to be in the columns.
% Each column is balanced w.r.t. the reference trace. trref may not be a
% matrix.
%
% by G.F. Margrave, June 1991 and 2009
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


if nargin<2
 trref=ones(size(trin));
end
if nargin<3
   if(length(trref)>=length(trin))
        tz=[1:length(trin)];
   else
        tz=[1:length(trref)];
   end
end 
if(~isvector(trref))
        error('trref must be a vector')
end
if(isvector(trin))
    %single channe case
    trout= trin*norm(trref(tz),2)/norm(trin(tz),2);
else
    trout=zeros(size(trin));
    
    top=norm(trref(tz),2);
    for k=1:size(trin,2)
        trout(:,k)=trin(:,k)*top/norm(trin(tz,k),2);
    end
end
  