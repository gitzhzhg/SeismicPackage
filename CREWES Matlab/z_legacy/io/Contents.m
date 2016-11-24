% The CREWES Seismic I/O toolbox
% A collection of functions to read and write segy seismic and logs
% in LAS or GMA format.
% 
% fmreadsegy   --- read segy into a fleximat
% fmwritesegy  --- write segy from a fleximat
% readsegy     --- read segy into an ordinary matrix
% writesegy    --- write an ordinary matrix as segy
% segyin_open  --- open a segy dataset for reading
% segyin       --- read in a trace from a segy dataset
% segyout_open --- open a segy dataset fro output
% segyout      --- write a trace to a segy dataset
% readgma      --- read a log in GMA format
% writegma     --- write a log in GMA format
% readlas      --- read a log in LAS format (reads LAS3 or earlier)
% writelas     --- write a log in LAS format
% readzmap     --- read a zmap grid
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
