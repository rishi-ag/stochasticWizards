# stochasticWizards
Collaboration on the stochastic optimization presentation.

1. Im attaching a url to an online 15 minutes tutorial to git. Will be useful for
fluency in working with git. Cheers https://try.github.io/levels/1/challenges/1


2. These are the options to fly the drone and its implementation:

a) GPS data options(I guess to be implemented at the controller function)

	-No online GPS measures: Open policy should not update the position at each 
    step, should control drone as if in every step we were precisely at the waypoint.
    It's a 'blind' flight regarding position

	-Exact GPS measures: Perfect state, should update the position at each step,
    considering the dynamics

	-Uncertain GPS measures: Imperfect state, we are not observing actual X, but
    approximate X' coordinates
	
b) Wind data options (to be implemented at the wind model function)

	-No wind simulation will be a simulation that eliminates wind component (all
    components set to zero)

	-Fix wind simulation will be assuming a constant wind (by default the real 
    initial one)

	-Simulated would be simulate wind (just considering an initial wind info if 
    exists, by default the real initial one) with a stochastic autorregressive 
    model. The controller will decide every move with the expected value, whereas
    the dynamics will be affected by expected + random value of wind.

	-Online data would simulate wind as if at each step we knew the actual wind data,
    and so we just have to simulate the random part.

c) Simulated vs real result (I think we should always plot simulated vs real result)

	-Simulated is the forecast result of the path according to the model (output of
    the controller function)

	-Real is the actual result of the path given the actual data we had (to be 
    implemented in a function that considers actual wind and simulated controls)
	
3. As outcome, we should do Montecarlo runs of the different stochastic options 
and plot expected loss, vs actual loss with real result. These should prove the 
performance of an increasing control on the drone.
