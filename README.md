# leastcostpath - Version 0.1

R Implementation of Least Cost Path Analysis Functions

<b>current functions</b>
* <code>leastcostpath</code> - computes Least Cost Paths using multiple cost functions.</b>

<u>Cost Functions Implemented</u></b>

* Tobler's Hiking Function (1993)</b> 
<code>6 * exp(-3.5 * abs(slope[adj] + 0.05))</code></b>


* Marquez-Perez et al. (2017) Modified Hiking function</b> 
<code> 4.8 * exp(-5.3 * abs(slope[adj] * 0.7) + 0.03)</code></b> 

* Llobera and Sluckin (2007) Fourth-degree polynomial function</b> 
1/(1 + abs(slope[adj]/crit_slope)^2)</b> 
  
  

* <code>validation_buffer</code> - computes the accurracy of the Least Cost Path relative to another SpatialLine* object based on method proposed by Goodchild and Hunter (1997).

<b> Future functions</b>
* Incorporate 24 neighbours within LCP calculation. 60% complete.
* Implement validation method based on the distance from the optimal route (ie. straight line) and the LCP. 20% complete

# History

<b>Installation:</b>

<code>#install.packages("devtools")</code><br />
<code>library(devtools)</code><br />
<code>install_github("josephlewis/leastcostpath")</code><br />
<code>library(leastcostpath)</code>

# History

<code>Version 0.1</code> First Release to Github

