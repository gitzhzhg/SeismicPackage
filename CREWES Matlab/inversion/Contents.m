% The CREWES Inversion toolbox
%
% Twoo different 1-D inversions schemes (BLIMP amd TRIMP), band limited integration,
% computing Rcs from impedance and the reverse.
%
% BLIMP ... Band limited impendance inversion using specific frequency band
%           from well logs
% BLINT   ... Band limited integration
% IMP2RCS ... Given impendance, compute RC's (reflection coefficients)
% IMP2RCS1 ... exact conversion of impedance to RCS
% IMPERROR ... estimate the error in an impedance calculation
% MERGETRCS ... Merge two seismic traces in the frequency domain (used by
%              BLIMP)
% RCS2IMP ... Given RC's, compute impendance
% RCS2IMP1 ... exact conversion of RC's to impedance
% RCS2IMP_R ... Recusive integration of RC's
% RCSERROR ... estimate the error in an estimation of RC's
% TRIMP ... bandlimited impedance inversion use trend from well logs
%
% (C) The CREWES Project, 2016  
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
