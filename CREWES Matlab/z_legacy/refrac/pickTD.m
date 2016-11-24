% Picks the CVP from the time-difference curve, and returns the
% intermediate results
%
% Input:  tdiff       - Valid time difference curve
%         tdcoord     - coordinates of all time difference points
%         window      - median filter window length
%         nd          - difference gap
%         offsetrange - (1) is minimum offset, (2) is maximum offset
%         shotcoord   - coordinate of the shot
%
% Output: 
% Note that the returned derivatives (deltf and delts) have the
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

% coordinates as the 2nd row (deltf(2,:), delts(1,:) )
function [mddiff, deltf, delts, cvp] = pickTD(tdiff, tdcoord, window, ...
                         windmn, nd, offsetrange1, offsetrange2, shotcoord)
% The traveltime difference on the left side of the left shot (i) can not be 
% empty to allow the filtering and the derivation
 
mddiff=[]; deltf=[]; delts=[]; cvp=[];
if( length(tdiff)>0)
   win=window;
   [o tdlen]=size(tdiff);
   % the filtering window can't be larger than the size of
   % the traveltime difference
   if (tdlen < win)
         win=3;
   end
   r=(win*0.5)-0.5;
   l=tdlen-r+1;
 
   % Median filtering of the traveltime difference
   mddiff=medfilt1(tdiff,win);
 
   % Replace the beginning and the end of the filtered TD curve with
   % the original TD.
   if (tdlen >= 2*r)
     mddiff([1:r,l:tdlen])=tdiff([1:r,l:tdlen]);
   end
   tdcend = length(tdcoord);
   % First derivative of the filtered traveltime difference
   if (tdlen>2*nd)
      delttf = jddiff(mddiff, nd);
      deltxf = jddiff(tdcoord, nd);
      deltxf = abs(deltxf);
      avgxf = (tdcoord(1:tdcend-nd) + tdcoord(1+nd:tdcend) ) / 2;
      deltf = delttf ./ deltxf;
      if(isnan(windmn)==0)
        deltf = menfilt1(deltf, windmn);
      end
      deltf(2,:) = avgxf;
   else
      deltf = [];
   end
   % Second derivative of the filtered traveltime difference
   [a b] = size(avgxf);
   if( b > 2*nd )
      delts = jddiff( deltf(1,:), nd );
      avgxs = ( avgxf(1:b-nd) + avgxf(nd+1:b) ) / 2;
      delts(2,:) = avgxs;
   else
      delts = [];
   end
   % Pick the cvp from the second derivative of the traveltime difference
   if (~isempty(delts))
      if( ~isnan(offsetrange1) & ~isnan(offsetrange2) )
         offsetmin = offsetrange1;
         offsetmax = offsetrange2;
         offset = abs(shotcoord - avgxs);
	 goodind = find( offset > offsetmin & offset < offsetmax );
         if(~isempty(goodind))
            [a, maxindex] = max(delts(1,goodind));
            cvp = avgxs(goodind(maxindex));
         end
      else
         [a, maxindex] = max(delts(1,:));
         cvp = avgxs(maxindex);
      end
   end
else
   mddiff1 = [];
   deltf1  = [];
   delts1 = [];
   cvp = NaN;
end