% CREWES raytracing tools
% Raytracing for v(z) and v(x,z)
%
%Demos and scripts
% RAYTRACE_DEMO ... interactive demonstration of v(z) raytracing capabilities
% RAYVXZ_DEMO ... demo the v(x,z) raytrace code
% RAYMARMOUSI_DEMO ... demo the v(x,z) raytrace code on the Marmousi model
%
%Basic tools for v(z) 
% DRAWRAY ... draws rays given their ray parameters
% RAYFAN_A ... similar to RAYFAN but the rays are specified by angle
% RAYFAN ... shoots a fan of rays given their ray parameters for v(z)
% SHOOTRAY ... similar to RAYFAN but with less error checking (faster)
% SPHDIV ... compute the geometrical spreading factor for v(z)
% TRACERAY ... traces an arbitrary ray given its raycode for v(z)
% TRACERAY_PP ... traces a P-P (or S-S) reflection for v(z)
% TRACERAY_PS ... traces a P-S (or S-P) reflection for v(z)
%
%Tools for v(x,z)
% DRAYVEC ... compute the derivative of ray vector (for vxz raytracing)
% DRAYVECLIN ... compute the derivative of ray vector for v0=a*x+b*z
% RAYVELMOD ... establish a velocity model for vxz raytracing
% SHOOTRAYTOSURF ... shoot a ray to z=0 in v(x,z)
% SHOOTRAYVXZ ... RK4 raytracing in v(x,z) with nearest neighbor int.
% SHOOTRAYVXZ_G ... more general raytracing in v(x,z).
%
%Normal raytracing for v(x,z)
% CLEARRAYS ... clear (delete) the rays in a figure
% CLEARPICKS ... clear (delete) the picks in a figure
% EVENTRAYMIG ... raytrace migrate a picked event assuming normal incidence
% EVENTRAYMOD ... raytrace model a picked event assuming normal incidence
% NORMRAY ... trace a normal ray to the surface
% NORMRAYMIG ... migrate a normal incidence ray
% RAYVELMOD ... initialize a velocity model for normal ray tracing
%
%Traveltime calculation
% EIKONAL2D ... calculate traveltimes for a 2D velocity model
%
% The next files are in the old CREWES tooldbox. Use with caution
%
