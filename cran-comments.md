## Test environments
* local OS Windows >= 8 x64 (build 9200) install, R 3.4.1 (2017-06-30)
* Ubuntu Trusty 14.04 (on travis-ci), R version 3.6.2 (2017-01-27)
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs when submitting to win-builder:

0 errors √ | 0 warnings √ | 3 notes √

* Possibly mis-spelled words in DESCRIPTION:
  leastcostpath (2:8)
  
    leastcostpath is the name of the R package

* running examples for arch 'i386' ... [59s] NOTE
  Examples with CPU or elapsed time > 10s
                    user system elapsed
  create_FETE_lcps   17.44   2.30   19.92
  create_lcp_density 16.35   1.98   18.39

    Examples of function usage dependent on prior calculated steps. 
    
* running examples for arch 'x64' ... [60s] NOTE
  Examples with CPU or elapsed time > 10s
                    user system elapsed
  create_FETE_lcps   16.34   3.00   19.41
  create_lcp_density 15.32   1.64   16.96    
  
    Examples of function usage dependent on prior calculated steps.   
