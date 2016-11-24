function [cvpi,cvpj,tdiff1, tdiff2, mddiff1, mddiff2, ...
          deltf1, deltf2, delts1, delts2, ...
          start1indexj, end1indexj, start2indexj, end2indexj] = ...
   autopick(gs1,gs2,shotcoord,fbcoord,fbtime,...
            window,nshots,nd,offsetrange1, offsetrange2, windmn)
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

% This function picks the cross over point coordinates according to 
% the maximum of the second derivative of the traveltime difference
% The cvpi correspond to the left cross over point of shot i
% The cvpj correspond to the right cross over point of shot j
cvpj=NaN .* ones(nshots,nshots);
cvpi=NaN .* ones(nshots,nshots);
% Sometimes the gs1 vector is a row, sometimes a column.  Handle this.
[a b] = size(gs1);
nshotpairs = max(a, b);
% Loop throught the specified shot pair(s)
for k=1:nshotpairs
   i = gs1(k);
   j = gs2(k);
   fprintf(1,'autopick: computing for shotpairs %d %d\n',i,j);
   % First call the traveltime substraction function to calculate
   % the traveltime difference on each side of the shot pair locations 
   
   tdiff1=[]; tdiff2=[]; start1indexj=[]; start2indexj=[];
   end1indexj=[]; end2indexj=[]; 
   [tdiff1, tdiff2, start1indexj, start2indexj, ...
    end1indexj, end2indexj, i, j] = tsub(i, j, shotcoord, fbcoord, fbtime);
    
   mddiff1=[]; deltf1=[]; delts1=[]; cvp1=[];
   [mddiff1, deltf1, delts1, cvp1] = pickTD(tdiff1, ...
       fbcoord(j,start1indexj:end1indexj), window, windmn, nd, ...
       offsetrange1, offsetrange2, shotcoord(i));
       
   mddiff2=[]; deltf2=[]; delts2=[]; cvp2=[];
   [mddiff2, deltf2, delts2, cvp2] = pickTD(tdiff2, ...
       fbcoord(j,start2indexj:end2indexj), window, windmn, nd, ...
       offsetrange1, offsetrange2, shotcoord(j));
   if(~isempty(cvp1))
      cvpi(i,j) = cvp1;
   end
   if(~isempty(cvp2))
      cvpj(i,j) = cvp2;
   end
end 