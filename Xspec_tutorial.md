# [Xspec](https://heasarc.gsfc.nasa.gov/xanadu/xspec/manual/XspecManual.html) Tutorial For Spectral Analysis

- Initialize HEASOFT:

```
heainit
```

- Initialize Xspec:

```
xspec
```

- Set up your environment first. Define your fit statistic method and set your terminal input to yes when the max number of iterations is reached during your spectral fitting instead of doing so manually:
```
statistic chi
query y
```
- Open XWindow to display your output and set your units to keV:
```
cpd /xw
setplot e
```
- Load your spectrum, filename.fits:

```
data 1:1 filename.fits
```
- Load your second spectrum, if any:

```
data 2:2 filename_2.fits
```

- Ignore energy ranges your instrument is not sensitive to:
  
  <code>low_e</code> : the lowest energy of your range
  
  <code>high_e</code> : the highest energy of your range

For spectrum 1:

```
ignore 1:**-low_e high_e-**
```

For spectrum 2:

```
ignore 2:**-low_e high_e-**
```

- Plot your data in logarithmic scale:

```
pl ld
```

- Define your model:

```
model tbabs*(diskbb+po)
```

- Fix your parameters (if needed):

```
newpar par_no value -1
```

- Set your parameter free:

```
thaw par_no
```

- Link your parameter (par_no) to another parameter (par_no2), if needed:

```
newpar par_no=par_no2
```

  e.g. if you have a model as tbabs*(diskbb+nthcomp) in which you have a common parameter Tin(2nd parameter in the model)/kT_bb(6th parameter in the model), you can link them as follows:

```
newpar 6=2 
```

- Start the fit:

```
fit
```

- Check your results and calculate your errors:
  
  Calculate errors with a 90% confidence interval for a specific parameter with parameter number par_no:
  
```
error par_no
```

  Calculate errors for all of the parameters:
  
```
error 1-last_par
```

- Plot your best-fit model, data and residuals:

  Data: <code>data</code>, <code>ldata</code> (logarithmic scale)

  Unfolded: <code>ufspec</code>, <code>eufspec</code>, <code>eeufspec</code> 
  
  Residuals: <code>res</code>, <code>ratio</code>, <code>chi</code>, <code>delchi</code> 

  Plot your model components along with the total best-fit model:
```
setplot add 
```
  Plot your choice of data and residuals:

```
pl ld delchi
```
- Rescale your axes:


```
setplot com r x 0.3 10.0
setplot com r y 1.e-2 1.e+32
```

- If you have residuals in your spectrum, e.g. an Fe K 	$\alpha$ emission line and your fit statistic needs improvement you can add a gaussian line to the defined model above:

```
addcomp 3 gauss
```

  Warning: this is to show addition of a simple component, reflection components such as strong Fe K 	$\alpha$ emission line and a Compton hump dominating at higher energies require a more detailed and self-consistent modelling of relativistic reflection if it originates from the accretion disk. These models include <code>relxill</code>, <code>xillver</code> and more. Check these models for information if you are interested.

- Save your results:

  Options: <code>all</code> (saves your model, parameter values and fit statistic of your model and data with all settings applied above), <code>model</code> (saves the best-fit model with parameter values), <code>files</code> (saves your data files)
  
```
save <option> file_name
```
  
  This saves the choice of options to file_name.xcm which you can later use to reload your progress:
 
 ```
@file_name.xcm
```

- Modify your plot:

  Color numbers for PGPLOT:
<img width="528" alt="Pasted Graphic 5" src="https://github.com/anastasiyayilmaz/Private/assets/57295156/a187db2d-b6c6-46cd-bdcd-e1d7a5ac6a37">
 
 Start PGPLOT within your Xspec session:
 
```
iplot
```
  Set color 11 on your spectrum 1:
  
```
CO 11 ON 1
```

  Depending on the number of parameters in your model, the following numbers will correspond to the model components of your spectrum 1, then your spectrum 2 and components for spectrum2 and so on...

  Add a label to your spectrum positioned at (2, 0.01) for your spectrum:
  
 ```
LA 1 POS 2 0.01 CO 11 "YOUR LABEL"
```

  Save a colored PostScript of your plot to /YOUR/FAVORITE/PATH when you are done:
  
   
```
hardcopy /YOUR/FAVORITE/PATH/filename.ps/cps
``` 

Et voilà! 

