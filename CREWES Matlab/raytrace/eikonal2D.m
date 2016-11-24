function TT_ZX = eikonal2D(V_ZX, dx, XS, ZS)
% TT_ZXY = eikonal2D(V_ZX, dx, XS, ZS)
%
% Fast-marching eikonal equation solver following Sethian & Popovici
% (Geophysics 64 no 2 pp 512-523, 1999)
%
% V_ZX: unpadded velocity model
% dx: grid spacing
% XS, ZS: shot location as an index into the V_ZX. Note that this is
% NOT a spatial location, but rather the index into the matrix!
% TT_ZXY: matrix of first arrival travel times, same size as V_ZX
%
% Chad Hogan, 2005
%
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

% $Id: eikonal2D.m,v 1.2 2009/07/24 19:50:51 gary Exp $

XS = XS+1;
YS = 2;
ZS = ZS+1;

% initialize a traveltime matrix

V_ZXY = ones(size(V_ZX, 1)+2, size(V_ZX, 2)+2, 3);
V_ZXY(2:(end-1), 2:(end-1), 2) = V_ZX;

TT_ZXY = zeros(size(V_ZXY));
TT_ZXY(ZS, XS, YS) = 0;

% initialize an accepted mask and a close mask
AccMask_ZXY   = zeros(size(V_ZXY));
CloseMask_ZXY = zeros(size(V_ZXY));
CloseMask_ZXY(ZS, XS, YS) = 1; % We start out with the source.

% build our boundaries mask
BoundMask_ZXY = ones(size(V_ZXY));
BoundMask_ZXY(1, :, :)   = 0;    % boundaries
BoundMask_ZXY(:, 1, :)   = 0;
BoundMask_ZXY(:, :, 1)   = 0;
BoundMask_ZXY(end, :, :) = 0;
BoundMask_ZXY(:, end, :) = 0;
BoundMask_ZXY(:, :, end) = 0;

% remove the boundaries from our close mask
CloseMask_ZXY = CloseMask_ZXY & BoundMask_ZXY;

S_ZXY = 1./V_ZXY; % slowness matrix

% Ok, now we have our mask that tells use where the Close values are
% and we also have a mask that tells us where our accepted values are.

dims(1) = size(V_ZXY,1);
dims(2) = size(V_ZXY,2);
dims(3) = size(V_ZXY,3);

% we need one step for each and every point in the domain, no more no less.
% We know our source point so we can skip that one.

steps = dims(1) * dims(2) * dims(3) - 1 - length(find(~BoundMask_ZXY));
calccount = 1;

for step = 1:steps
    % Now we have to find out where our minimum TT within the close region
    % lies.

    % Step one from S&P

    closeidxs = find(CloseMask_ZXY);
    Trials    = TT_ZXY(closeidxs);
    mintrial  = find(Trials == min(Trials));
    mintrial  = mintrial(1);
    Trial     = closeidxs(mintrial);

    %triallocs = eikind2sub(dims, Trial); siz ndx
    nout = 3;
    n = 3;

    k = [1 dims(1) dims(1)*dims(2) dims(1)*dims(2)*dims(3)];
    Trial = Trial - 1;

    v = floor(Trial/k(3))+1;
    triallocs(3) = v;
    Trial = rem(Trial,k(3));

    v = floor(Trial/k(2))+1;
    triallocs(2) = v;
    Trial = rem(Trial,k(2));

    v = floor(Trial/k(1))+1;
    triallocs(1) = v;
    Trial = rem(Trial,k(1));
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
    TrialZ    = triallocs(1);
    TrialX    = triallocs(2);
    TrialY    = triallocs(3);


    % steps two and three from S&P
    AccMask_ZXY(TrialZ, TrialX, TrialY) = 1;
    CloseMask_ZXY = setNeighbours(CloseMask_ZXY,TrialZ,TrialX,TrialY,1);

    
    % eliminate accepted values from CloseMask.
    CloseMask_ZXY = CloseMask_ZXY - CloseMask_ZXY .* AccMask_ZXY;
    CloseMask_ZXY = CloseMask_ZXY .* BoundMask_ZXY;

    % step four from S&P
    %closeidxs = find(CloseMask_ZXY .* V_ZXY ~= 0);
    closeidxs = find(CloseMask_ZXY .* V_ZXY);

    for idx = 1:size(closeidxs)

        % figure out upwind/downwind direction and then pick the
        % appropriate bits out of (9). Then write out the equations and
        % solve

        %cllocs = eikind2sub(dims, closeidxs(idx));
        Trial = closeidxs(idx);
        nout = 3;
        n = 3;

        k = [1 dims(1) dims(1)*dims(2) dims(1)*dims(2)*dims(3)];
        Trial = Trial - 1;

        v = floor(Trial/k(3))+1;
        cllocs(3) = v;
        Trial = rem(Trial,k(3));

        v = floor(Trial/k(2))+1;
        cllocs(2) = v;
        Trial = rem(Trial,k(2));

        v = floor(Trial/k(1))+1;
        cllocs(1) = v;
        Trial = rem(Trial,k(1));

        
        
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        ClZ = cllocs(1);
        ClX = cllocs(2);
        ClY = cllocs(3);

        weight = 0;

        Tx = 0;
        Ty = 0;
        Tz = 0;

        if(AccMask_ZXY(ClZ, ClX-1,ClY)) % wind blowing to the right
            Tx = TT_ZXY(ClZ, ClX - 1, ClY);
            weight = weight + 1;
        elseif(AccMask_ZXY(ClZ,ClX+1,ClY)) % wind blowing to the left
            Tx = TT_ZXY(ClZ, ClX + 1, ClY);
            weight = weight + 1;
        end

        if(AccMask_ZXY(ClZ,ClX,ClY-1)) % wind blowing to the back
            Ty = TT_ZXY(ClZ, ClX, ClY - 1);
            weight = weight + 1;
        elseif(AccMask_ZXY(ClZ,ClX,ClY+1)) % wind blowing to the front
            Ty = TT_ZXY(ClZ, ClX, ClY + 1);
            weight = weight + 1;
        end

        if(AccMask_ZXY(ClZ-1,ClX,ClY)) % wind blowing down
            Tz = TT_ZXY(ClZ-1, ClX, ClY);
            weight = weight + 1;
        elseif(AccMask_ZXY(ClZ+1,ClX,ClY)) % wind blowing up
            Tz = TT_ZXY(ClZ+1, ClX, ClY);
            weight = weight + 1;
        end

        S = S_ZXY(ClZ, ClX, ClY);

        c2 = -2*(Tx + Ty + Tz);
        c3 = Tx^2 + Ty^2 + Tz^2 - S^2 * dx^2;

        %TTs = roots([weight c2 c3]);
        sec = sqrt(c2^2 - 4 * weight * c3);
        TTs = [(-c2 + sec) / 2/weight, (-c2 - sec) / 2/weight];

        if (TTs(1) > TTs(2)) maximum = TTs(1);
        else maximum = TTs(1);
        end   
            
        TT_ZXY(ClZ, ClX, ClY) = maximum;

        calccount = calccount + 1;
    end
end

TT_ZX = TT_ZXY(2:(end-1), 2:(end-1), 2); 

display(['calculated points ' num2str(calccount) ' times']);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function Matrix = setNeighbours(Matrix, z, x, y, val);
% this function takes a matrix and "turns on" the neighbours in a mask.

[zl, xl, yl] = size(Matrix);

zidx = max(z-1,1):min(z+1, zl);
xidx = max(x-1,1):min(x+1, xl);
yidx = max(y-1,1):min(y+1, yl);

Matrix(zidx, x, y) = val;
Matrix(z, xidx, y) = val;
Matrix(z, x, yidx) = val;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function is a special version of ind2sub() that runs a whole lot
% faster in this special case situation. We spend a lot of time in this
% function, this version saves us about 25%.

function answers = eikind2sub(siz,ndx)

nout = 3;
n = 3;

k = [1 siz(1) siz(1)*siz(2) siz(1)*siz(2)*siz(3)];
ndx = ndx - 1;

v = floor(ndx/k(3))+1;
answers(3) = v;
ndx = rem(ndx,k(3));

v = floor(ndx/k(2))+1;
answers(2) = v;
ndx = rem(ndx,k(2));

v = floor(ndx/k(1))+1;
answers(1) = v;
ndx = rem(ndx,k(1));