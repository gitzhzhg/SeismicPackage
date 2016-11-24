% The CREWES Inversion toolbox
%
% Two different 1-D inversion schemes, band limited integration,
% computing Rcs from impedance and the reverse.
%
% blimp   - Band limited impendance inversion
% blint   - Band limited integration
% imp2rcs - Given impendance, compute RC's
% mergetrcs - Merge two seismic traces in the frequency domain (used by
%              blimb)
% rcs2imp - Given RC's, compute impendance
% rcs2imp_r - Recusive integration of RC's
%
% demo:
% demo_impinv - uses a real well log to demonstrate the inversion process
%           and the importance of low frequencies.
%
% (C) The CREWES Project, 1996  
%

