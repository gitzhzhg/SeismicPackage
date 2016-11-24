% The CREWES Geometric toolbox
% These are routines for geometric problems such as 'find the closest point
% to a line', 'is a point inside a polygon', etc.
% 
% All these routines work with piecewise linear curves.
%
% arclen2xy   --- converts from arclength representation to xy coordinates of 
%             --- piecewise linear curve
% between     --- logical test for 'betweenness'
% ccw         --- counterclockwise - sorts lines which intersect at point in
%                 counterclockwise order
% closestnode --- finds the closest node on a PWL curve to a point
% closestpt   --- finds the closest point on a PWL to a given point
% distcum     --- computes the cumulative distance along a PWL
% distinc     --- computes the incremental distance along a PWL
% distpoint **--- computes the perpendicular distance from a point to a line
% distreach   --- given a pointset, finds all points with a radius of a
%                 given point
% inside      --- given a closed polygon determines whether a point is
%                 inside it
% notbetween  --- the logical opposite of between
% oncurve     --- determines if a point is on a PWL or not
% paratran    --- parallel transport of a point parallel to a PWL
% pdist **    --- 
% poly2d      --- evaluates a 2-D polynomial on a grid
% polyarea    --- computes the area of an polygon in 2-D
% polydist    --- computes the minimum distance between a polynomial and a line
% polyint     --- Polynomial interpolation
% polysurf    --- Fits a 2-D polynomial surface to data
% slope       --- Computes the slope at a given point on a PWL
% surround    --- Finds the indicies of points on a PWL which surround a
%                 given point 
% xy2arclen   --- Convert from xy representation to arc length representation
% ycurve      --- Computes the y coordinate of a PWL at a given x coordinate
%
% (C) The CREWES Project, 1996  
%
% NOTE: It is illegal for you to use this software for a purpose other
% than non-profit education or research UNLESS you are employed by a CREWES
% Project sponsor. By using this software, you are agreeing to the terms
% detailed in this software's Matlab source file.
 
% BEGIN TERMS OF USE LICENSE
%
% This SOFTWARE is maintained by the CREWES Project at the Department
% of Geology and Geophysics of the University of Calgary, Calgary,
% Alberta, Canada.  The copyright and ownership is jointly held by 
% its author (identified above) and the CREWES Project.  The CREWES 
% project may be contacted via email at:  crewesinfo@crewes.org
% 
% The term 'SOFTWARE' refers to the Matlab source code, translations to
% any other computer language, or object code
%
% Terms of use of this SOFTWARE
%
% 1) Use of this SOFTWARE by any for-profit commercial organization is
%    expressly forbidden unless said organization is a CREWES Project
%    Sponsor.
%
% 2) A CREWES Project sponsor may use this SOFTWARE under the terms of the 
%    CREWES Project Sponsorship agreement.
%
% 3) A student or employee of a non-profit educational institution may 
%    use this SOFTWARE subject to the following terms and conditions:
%    - this SOFTWARE is for teaching or research purposes only.
%    - this SOFTWARE may be distributed to other students or researchers 
%      provided that these license terms are included.
%    - reselling the SOFTWARE, or including it or any portion of it, in any
%      software that will be resold is expressly forbidden.
%    - transfering the SOFTWARE in any form to a commercial firm or any 
%      other for-profit organization is expressly forbidden.
%
% END TERMS OF USE LICENSE
