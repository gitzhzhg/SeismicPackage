% CREWES velocity manipulation toolbox
% Tools to convert velocity functions from one form to another.
% 
% IMPORTANT!!! Please read the following paragraph.
%There is a basic ambiguity when a velocity function is specified by a
%paired list of velocities and depths such as [v1 v2 v3] versus [z1 z2 z3].
%In this toolbox, WE ASSUME that this list specifies a step function of
%interval velocities for homogeneous layers. The depths are taken to be the
%tops of the layers. Thus the example list says that the first layer
%extends from z1 to z2 with constant velocity v1, the second layer goes
%from z2 to z3 with velocity z3, and then v3 applies to all depths below
%z3. For depths less than z1, the velocity is undefined. For a dense
%velocity specification such as from a well log, this hardly matters, but
%for a sparse specification, it is important to follow this assumed model
%to get correct results.
%
% DRAWVINT ... draw interval velocity as a piecewise constant function
% VAVE2VINT ... convert average velocity to interval velocity
% VINT2T ... compute time given interval velocity versus depth
% VINT2VAVE ... convert interval velocity to average
% VINT2VRMS ... convert interval to rms velocity
% VRMS2VINT ... convert rms to interval velocity
% VRMS2VAVE ... convert rms to average velocity
% VZMOD2VRMSMOD ... convert an interval velocity model in depth to vrms in time
% VELSMOOTH ... smooth a 2D velocity model by convolution with a 2D Gaussian
% VINS2TZ ... calculate time-depth curves from instantaeous v(z)
% VelocityAnalysis ... interactive velocity analysis tool for shot gathers
% GetVelocities ... returns velocites from the analysis tool
%
% DEMO_VAVE_VRMS_VINT_NOISE... edit this and run each cell consequtively
%
% Related tools: nmor and nmor_srm in the Seismic toolbox
%
% EXAMPLE: Compute two-wave traveltime, average velocity, and rms velocity
% given interval velocity versus depth. Let vint and z be vectors of the
% same length specifying a layered model as described above. Then:
% t=2*vint2t(vint,z);%step 1 computes 2-way traveltime to each depth in z
% vave=vint2vave(vint,t);%Step 2 gives us average velocity to each depth in z
% vrms=vint2vrms(vint,t);%Step 3 gives us rms velocities to each depth in z
%
% Plotting examples:
%
% plot a time-depth curve
% Figure;
% plot(t,z);
% xlabel('time (s)','Depth (m)')
%
% plot all three velocities versus time
% plot(t,vave,t,vrms)
% drawvint(t,vint,'k')
% legend('Average velocity','RMS velocity','Interval velocity')
%


