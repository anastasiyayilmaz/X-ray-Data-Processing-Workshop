# X-ray Data Processing Workshop, Silesian University Opava (19-21 June 2023)

A set of extra material intended for the X-ray Data Processing Workshop. Includes the following:

- A Jupyter notebook using [PyXspec](https://heasarc.gsfc.nasa.gov/xanadu/xspec/python/html/index.html) for spectral analysis
- A Jupyter notebook to produce science products from NuSTAR FPMA and FPMB using [nustarpipeline](https://gitlab.astro.unige.ch/ferrigno/nustar-pipeline), developed by Carlo Ferrigno (ISDC, University of Geneva).

Both notebooks assume you have successfully installed [HEASOFT](https://heasarc.gsfc.nasa.gov/docs/software/lheasoft/) and have your [CALDB](https://heasarc.gsfc.nasa.gov/docs/heasarc/caldb/caldb_intro.html) set up. To be able to use PyXspec, make sure you defined your Python.

Make sure you initialize HEASOFT first:


```
heainit
```


Check if you have a working PyXspec:


```
python3.9 -c 'import xspec'
```

 


If you receive no message, you are all set!

How to install nustarpipeline:

```
pip install nustarpipeline
```

or
```
python3.9 -m pip install nustarpipeline
```

You can run the threads from the notebook or use process_nustar.py. 

For usage and detailed options, run:

```
process_nustar.py
```




