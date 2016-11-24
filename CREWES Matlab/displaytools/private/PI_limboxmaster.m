function PI_limboxmaster()
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

h=get(gcf,'userdata');
hi=h{5};
hmaster=h{10};

hlimbox=h{14};
limfig=get(hlimbox,'userdata');
limmat=limfig{3};

ydat=sort([round(limmat(1)) round(limmat(2))]);
xdat=sort([round(limmat(3)) round(limmat(4))]);
% checking to see if lines numbers are acceptable, right now values
% will not change if the new lines are within 3 % of eachother
if((abs(ydat(1)-ydat(2)))/(ydat(1)+ydat(2))*100<=3)||((abs(xdat(1)-xdat(2)))/(xdat(1)+xdat(2))*100<=3)
    return
end
seis=get(hi,'cdata');
seis2=seis(ydat(1):ydat(2),xdat(1):xdat(2));
mxs=full(max(max(abs(seis2))));
smean=full(mean(mean(seis2)));
stddev=full(sqrt( sum(sum((seis-smean).^2 ) )...
    /numel(seis)));
set(hmaster,'userdata',[mxs smean stddev]);
plotimage('rescale');