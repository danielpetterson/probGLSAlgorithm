<!DOCTYPE html>

<html>

<head>

<meta charset="utf-8" />
<meta name="generator" content="pandoc" />
<meta http-equiv="X-UA-Compatible" content="IE=EDGE" />

<meta name="viewport" content="width=device-width, initial-scale=1" />

<meta name="author" content="Daniel Petterson" />


# probGLSAlgorithm

<div id="motivation" class="section level1">
<h1>Motivation</h1>
<p>The primary motivation behind the development of this package was to eliminate friction points, streamline data formatting and expand the capabilities of the existing packages that are used to estimate locations via light-level loggers/global location sensor (GLS) tags. Due to the hardware limitations of some models of GLS tags, namely their inability to record light intensity beyond a small range, the algorithm used in <code>probGLS</code> <span class="citation">(<a href="#ref-merkel2016probabilistic" role="doc-biblioref">Merkel et al. 2016</a>)</span> provides more reliable estimates than the methods available in other packages for tracking species that exist in areas of low shading. This method is used to great effect by the <a href="https://seapop.no/en/seatrack/">SEATRACK project</a> to map the distribution and flight patterns of a variety of seabirds native to the North Atlantic and neighbouring areas. Last summer I worked with the Department of Conservation to assess the viability of using this method to estimate the location and frequency of artificial light events for a variety of species native to New Zealand. While the algorithm has been shown to provide accurate estimates under the right conditions, limitations in the functionality and applying it to species that had a much larger range than previously used led to issues.</p>
<div id="issues-with-existing-packages" class="section level2">
<h2>Issues with existing packages</h2>
<div id="land-mask" class="section level3">
<h3>Land mask</h3>
<p>The land mask is essentially a filter that doesn’t allow location estimates to be generated within the boundaries specified. In the traditional usage of this it means that location estimates will not exist on any land mass but this can also be reversed so that estimates only occur on land. <code>probGLS::prob_algorithm</code> allows for some preset extensions to the land mask and users are able to add the Mediterranean, Black, Baltic and Caspian seas. One of the goals of this package was to expand that functionality and allow for users to input custom land mask augmentations. <code>probGLSAlgorithm::modify.land.mask</code> features the following additional preset options:</p>
<ul>
<li>Arctic Ocean</li>
<li>North Atlantic Ocean</li>
<li>South Atlantic Ocean</li>
<li>North Pacific Ocean</li>
<li>South Pacific Ocean</li>
<li>Southern Ocean</li>
</ul>
<p>In addition to these the function also takes in custom coordinates as defined by minimum longitude, maximum longitude, minimum latitude, maximum latitude. The output of <code>probGLSAlgorithm::GLS.prob.algorithm</code> includes a map which displays the coverage of the land mask applied for visual confirmation of the area that the custom land mask includes should it be altered. The land mask is important due to the nature of the algorithm which takes the geographic median point of each new set of location estimates as the next point in each track/iteration and the most probable path is calculated as the median path of all tracks/iterations. During the aforementioned study we had issues with location estimates occuring on the other side of North America which is impossible and occured because there was no way to restrict estimate locations besides narrowing the bounding box, the expected range of the birds.</p>

![](https://danielpetterson.github.io/assets/img/SootyShearwater.png)

<p>The attached image displays the issue clearly. The bounding box needed to allow for tracking around the southern tip of South America where the birds were known to have previously traveled but this also allowed for the estimates to be generated off the east coast of North America. With the ability to fine tune the allowed area via <code>probGLSAlgorithm::modify.land.mask</code> this is no longer an issue and skewing is less problematic.</p>
</div>
<div id="differences-in-formatting-between-devices-and-manufacturers" class="section level3">
<h3>Differences in formatting between devices and manufacturers</h3>
<p>The frequency of recordings and recorded values differ between devices and its not uncommon for studies to use a mixture of models at times when availability was limited due to legal and manufacturing issues. The <code>read.x</code> series of functions was written to simplify converting the raw data into the format required for location estimation. This may be especially useful because documentation on older models is difficult to come by and often lacking.</p>
</div>
</div>
</div>
<div id="the-process-of-development" class="section level1">
<h1>The process of development</h1>
<div id="initial-concept" class="section level2">
<h2>Initial concept</h2>
<p>The package was clearly inspired by my previous work with the related packages. Through that I identified multiple modifications that I thought would be most useful to someone trying to do similar work. I had planned to include a much larger number of <code>read.x</code> functions for specific device models but realised that I could condense them quite easily. Some devices haven’t been available for sale in over a decade and with the non-user replaceable batteries it was unlikely that there was a need to cover their specific formatting. I wanted the main function <code>GLS.prob.algorithm</code> to be easily swappable with <code>probGLS::prob_algorithm</code> to minimise the work required if a user wanted to migrate from the original version to take advantage of some of the functionality.</p>
</div>
<div id="implementation-of-ideas" class="section level2">
<h2>Implementation of ideas</h2>
<p>This part was relatively straightforward with the exception of the sliding bounding box in the main function which I couldn’t manage to implement in a time/processing efficient manner. A much quicker fix was to subset the output of <code>GLS.prob.algorithm</code> and generate a new geographic median value that only took into account locations within a given hemisphere over a user-specified time frame. This functionality is found in the <code>geo.median.track</code> function. This method is less than ideal as instead of using the median values of <span class="math inline">\(n\)</span> iterations the median is often calculated from <span class="math inline">\(&lt;&lt;n\)</span> values.</p>
</div>

<div id="deployment" class="section level2">
<h2>Deployment</h2>
<p>The package has been updated and can be found <a href="https://github.com/danielpetterson/probGLSAlgorithm">here</a> but users still need to download a handful of auxiliary data files that were not included due to their size and the requirement that the data files match the time covered in the recordings.</p>
</div>
</div>
<div id="difficulties-with-the-package-development" class="section level1">
<h1>Difficulties with the package development</h1>
<div id="lack-of-open-access-data" class="section level2">
<h2>Lack of open-access data</h2>
<p>Data for package testing needed to be generated based on samples as it wasn’t possible to find publicly available sources of data that fit the requirements. This is partially due the the very niche purpose of this type of equipment and partially due to it not often being used to track species that have a large latitudinal range. Reference data sets are available in <code>probGLS</code> but are small and already formatted for input into <code>prob_algorithm</code>. 

Note: The <code>read.sensor</code> function will process C65-SUPER tag data and will probably work on other models by the same company but files were not available for testing.</p>

</div>


<div id="dependencies" class="section level2">
<h2>Dependencies</h2>
<p>I attempted to minimise the number of dependencies used unless they showed a clear benefit such as <code>terra</code>/<code>tidyterra</code> which are significantly faster than the <code>raster</code> alternatives. A handful of operations were problematic to get working in base R so <code>dplyr</code> was required. This seemed a reasonable trade off as the tidyverse has continued support and most R users would already have <code>dplyr</code> installed. I opted to use the base R piping function instead of the more commonly seen <code>magrittr</code> pipe which does mean that a more recent version of R is required. 
</p>
<div id="refs" class="references csl-bib-body hanging-indent">
<div id="ref-merkel2016probabilistic" class="csl-entry">
Merkel, Benjamin, Richard A Phillips, Sébastien Descamps, Nigel G Yoccoz, Børge Moe, and Hallvard Strøm. 2016. <span>“A Probabilistic Algorithm to Process Geolocation Data.”</span> <em>Movement Ecology</em> 4 (1): 1–11.
</div>
</div>
</div>
</div>
</div>
</div>



<!-- code folding -->


<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>

</body>
</html>
