function StGrSour(xyzInd,ampf,iShX,iShY,iShZ,ngx,ngy,ngz,ixA,iyA,izA,leng)
%function StGrSour(xyzInd,ampf,iShX,iShY,iShZ,ngx,ngy,ngz,ixA,iyA,izA,leng)
%Plot a limited size (point) source for one component of a 3D staggered grid
%   against a light grey full staggered-grid
%
%The input parameters are
%xyzInd  .... Component indicator (X, Y, or Z)
%ampf    .... The relative indices of source forces in the xyzInd direction
%iShX    .... The shift of the ampf X-components from the 'source point'
%iShY    .... The shift of the ampf Y-components from the 'source point'
%iShZ    .... The shift of the ampf Z-components from the 'source point'
%ngx     .... Size of background staggered grid in the X-direction
%ngy     .... Size of background staggered grid in the Y-direction
%ngz     .... Size of background staggered grid in the Z-direction
%ixA     .... The relative X-position within the background staggered-grid
%iyA     .... The relative Y-position within the background staggered-grid
%izA     .... The relative Z-position within the background staggered-grid
%leng    .... Arrow lengths in the background staggered-grid
%
% P.M. Manning, Dec 2011
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
if(strcmpi(xyzInd,'X'))         %'X' displacement vector parms
    xzAng = pi/2; xyAng = 0;
    xSt = 0.5; ySt = 0; zSt = 0.5;  %Grid start for 'X'
    clr = 'b';
end
if(strcmpi(xyzInd,'Y'))         %'Y' displacement vector parms
    xzAng = pi/2; xyAng = pi/2;
    xSt = 0; ySt = 0.5; zSt = 0.5;  %Grid start for 'Y'
    clr = 'r';
end
if(strcmpi(xyzInd,'Z'))         %'Z' displacement vector parms
    xzAng = 0; xyAng = 0;
    xSt = 0; ySt = 0; zSt = 0;  %Grid start for 'Z'
    clr = 'k';
end
%Plot out background staggered component in this grid direction
ampf = ampf*0.75;
[zmain(1,:),zmain(2,:),zmain(3,:)] = gen3Arr(ngx,ngy,ngz,xSt,ySt,zSt);
arrow3(zmain(1,:),zmain(2,:),zmain(3,:),xzAng,xyAng,leng,[.9,.9,.9]) %'g')
ngxy = ngx*ngy;
%arrow3(zmain(1,1:ngxy),zmain(2,1:ngxy),zmain(3,1:ngxy),xzAng,xyAng,leng,'g')
hold on
if ampf~=0
    sz = size(ampf);
    [nArro,iDim] = max(sz);
    %disp([nArro,iDim])
    if nArro > 1
        if iDim==1
            if(iShX<0); ixA=ixA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
            iV2 = iV1+1;
            arrow3(zmain(1,iV2),zmain(2,iV2),zmain(3,iV2),xzAng,xyAng,....
                ampf(2,1,1),clr,'-',2)
        end
        if iDim==2
            if(iShY<0); iyA=iyA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
            iV2 = iV1+ngx;
            arrow3(zmain(1,iV2),zmain(2,iV2),zmain(3,iV2),xzAng,xyAng,....
                ampf(1,2,1),clr,'-',2)
        end
        if iDim==3
            if(iShZ<0); izA=izA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
            iV2 = iV1+ngxy;
            arrow3(zmain(1,iV2),zmain(2,iV2),zmain(3,iV2),xzAng,xyAng,....
                ampf(1,1,2),clr,'-',2)
        end
    else
        %if iDim==1
        if(strcmpi(xyzInd,'X'))         %'X' displacement vector parms
            if(iShX<0); ixA=ixA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
        end
        %if iDim==2
        if(strcmpi(xyzInd,'Y'))         %'Y' displacement vector parms
            if(iShY<0); iyA=iyA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
        end
        %if iDim==3
        if(strcmpi(xyzInd,'Z'))         %'Z' displacement vector parms
            if(iShZ<0); izA=izA-1; end
            iV1 = (izA-1)*ngxy+(iyA-1)*ngx+ixA;
            arrow3(zmain(1,iV1),zmain(2,iV1),zmain(3,iV1),xzAng,xyAng,....
                ampf(1,1,1),clr,'-',2)
            hold on
        end
    end
end