function ind = closetraceazi(xxx,yyy,xw,yw,rd,azi) 
% function ind = closetraceazi(xxx,yyy,xw,yw,rd,azi) 
%
%  finds closest trace (point) in line to given well location (xw,yw)
%  (as long as the trace is within a radius rd)
%  the distance MUST be defined along the azimuth azi, given in degrees
%  line defined by points xxx(i),yyy(i)
%  returns index of closest trace 
%  returns null if no trace is close enough
%
%  T. N. BISHOP,  DECEMBER 1993,  CPTC CANADA
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
    ind = [];
    dclose= closeline(xxx,yyy,xw,yw);
    if dclose <= rd
      mm = tan(2*pi*azi/360.);
      bb = yw - mm*xw;
      dmin = inf;
      m = diff(yyy)./diff(xxx);
      b = yyy(2:length(yyy)) - m.*xxx(2:length(xxx));
      xc = (bb - b)./(m - mm);
      yc = m.*xc + b;
      for i = 1:(length(xxx)-1) 
        i1 = i + 1;
        if ~isempty(between(xxx(i),xxx(i1),xc(i),2)) 
          if ~isempty(between(yyy(i),yyy(i1),yc(i),2)) 
%   find closest trace (point in xxx,yyy) to point
            if abs(xc(i)-xxx(i)) < abs(xc(i)-xxx(i1))
 	      ind = i;
            else
	      ind = i1;
 	    end
            d=sqrt( (xxx(ind)-xw).^2 + (yyy(ind)-yw).^2 );
            if(d < dmin)
              dmin = d;
      	      indmin = ind;
	    end
          end
        end
      end
%      plot(xxx(indmin),yyy(indmin),'c*')  plot closest trace point
      if dmin <= rd   %min.dist of all points is less than radius
        ind = indmin;
      else      %min.dist along azimuth not less than radius
        ind = [];
      end
    else
      ind = [];   %not close enough to even check
    end