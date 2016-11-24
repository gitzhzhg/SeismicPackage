function [vspdown,vspup,vspupflat,tpup,rdown]=VSP_separation(vsp,t,zr,tp,nmed,nmix,npasses)
% VSP_separation: separate a VSP into downgoing and upgoing waves
% 
% [vspdown,vspup,vspupflat,tpup]=VSP_separation(vsp,t,zr,tp,nmed,nmix,npasses)
%
% VSP_separation separates a VSP wavefield into downgoing and upgoing waves.
% The separation procedure is the conventional one of (1) flattening the
% downgoing primary wave by applying the first break picks as static
% shifts (see VSP_flatten), (2) applying a spatial median filter to
% suppress the upgoing wave, (2b) optionally applying a trace mix, (3)
% subtracting the median filtered, flattened, wavefield from the initial
% flattened wavefield. The result of (3) represents the estimate of the
% upgoing wave but flattened on the downgoing primary. Unflattening the
% results of (3) and (2) gives estimates of the upgoing wave and the
% downgoing wave in original time. Given the first break picks, it is also
% possible to flatten the upgoing wave. Thus the three wavefields returned
% are the downgoing and upgoing in original time and the upgoing flattened.
% At present the option exists to iterate the procedure of steps 2-3 any
% number of times. Testing suggests there is not much benefit to this.
%
% vsp ... input vsp matrix. One trace per column
% t   ... time coordinate for vsp
% zr  ... depth coordinate for vsp. Currently this is not used so you can
%         make it anything. 
% tp  ... vector of first arrival times for the primary downgoing wave.
%         There must be one value per trace. You can use PICKER for this.
% Requirement: length(t) must equal size(vsp,1) AND length(tp) must equal
%               size(vsp,2).
% nmed ... number of points in the median filter. This should be either a
%           scalar or a vector of length 2. If a scalar then the median
%           filter length is the same in all passes. If a length 2 vector,
%           then the first number is for pass 1 and the second number is
%           for the last pass. Intermediate passes will use an interpolated
%           value. See VSP_medfilt for details.
%   Suggestion: an odd number greater than 5.
% nmix ... Size of the spatial mix (equally weighted) applied immediately
%   after median filtering. This should be either a scalar or a vector
%   of length two. the values are used as described with nmed. See tracemix
%   for details on the mixing. Its good to make this an odd number.
% *************** default =1 (no mixing) ***************
% npasses ... number of passes
% *************** default = 1 *************
% 
% vspdown ... downgoing wave
% vspup   ... upgoing wave
% vspupflat ... flattened upgoing wave
% tpup ... vector of time shifts needed to flatten upgoing wave using VSP_flatten
%
%
% G.F. Margrave, 2015, CREWES
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

if(nargin<7)
    npasses=1;
end
if(nargin<6)
    nmix=1;
end

if(length(nmed)<2)
    nmed=[nmed nmed];
end
if(length(nmix)<2)
    nmix=[nmix nmix];
end
%flatten the downgoing wave
vspflat=VSP_flatten(vsp,t,tp,1,2);
%fine tune the flattening with crosscorrelations
[vspflat,dtau]=flatten(vspflat,t,zr,mean(tp),mean(zr),.02);
tp=tp-dtau;
vspflat=VSP_flatten(vsp,t,tp,1,2);
vspdownflat=zeros(size(vspflat));
for k=1:npasses
    if(k>1)
        nmedk=2*floor(interp1([1 npasses],nmed,k)/2)+1;
        nmixk=2*floor(interp1([1 npasses],nmix,k)/2)+1;
    else
        nmedk=nmed(1);
        nmixk=nmix(1);
    end
        
    %median filter 
    vspdownflatk=VSP_medfilt(vspflat,nmedk);
    if(nmixk>1)
        vspdownflatk=tracemix(vspdownflatk,ones(1,nmixk));
    end
    vspflat=vspflat-vspdownflatk;
    vspdownflat=vspdownflat+vspdownflatk;
end


%unflatten for downgoing wave estimate in true time
vspdown=VSP_flatten(vspdownflat,t,tp,-1,2);
rdown=VSP_flatten(vspflat,t,tp,-1,2);%residual downgoing wave

%remove downgoing wave from total field
vspup=vsp-vspdown;

%flatten upgoing wave

dtp=diff(tp);
tpup=fliplr([tp(end), tp(end)+cumsum(fliplr(dtp))]);
% tpup=flipud(2*tp(:));
vspupflat=VSP_flatten(vspup,t,tpup,1,1);
