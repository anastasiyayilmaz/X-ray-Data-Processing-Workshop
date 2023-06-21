# NuSTAR Tutorial

Some properties of NuSTAR:

Two focal planes: FPMA and FPMB with an energy resolution of 0.4 keV
Energy Range: 3-79 keV

## Data Reduction

Start with initializing HEASOFT, if you haven’t done already.

```
heainit
```

Each step will take quite a while, especially Stage-3. 

### Standard Data Processing Stage-1 and Stage-2:

The <code>nupipeline</code> script automatically runs a sequence of main NuSTAR tasks for data calibration, screening and preparing data for Stage-3 processing which involves extraction of science products such as lightcurves and spectra. A full list of tasks the script runs with more detailed descriptions can be found at https://heasarc.gsfc.nasa.gov/lheasoft/ftools/caldb/help/nupipeline.html. 

You can execute the script by:

```
nupipeline indir=/archive/10012001002/ steminputs=nu10012001002 outdir=./pipeline_out
```

By default, the task is executed for each focal plane FPMA and FPMB but one can run it for a specific one by adding <code>instrument=FPMB</code> to the above command. The script exits at Stage-2 (screening) by default but one can use <code>exitstage=1</code> to terminate at Stage-1(Calibration). For our analysis, we will run the script for both stages.

### Standard Data Processing Stage-3:

This step takes the outputs produced by <code>nupipeline</code> to extract higher-level science products by running another NuSTAR script nuproducts. It automatically retrieves the required input files from the directory created by nupipeline. 

Before running this script, you will need to define your source and background regions for each focal plane, FPMA and FPMB separately. 

Load the cleaned event file into ds9:

```
ds9 *A01_cl.evt
```

Define your source and background regions:

<p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/f3a2f233-efd6-4635-a51f-96ed268f2a7e">
</p>

Select your background from a nearby source-free region. 

Save your regions as sourceA.reg and background.reg for your source and background for FPMA respectively.

<p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/7c4af961-6236-4c36-9eb1-95f971224664">
</p>

Make sure to check these regions by loading them on the FPMB cleaned event too.

Below is an example to run nuproducts for FPMA, don’t forget to run it for FPMB as well:

```
nuproducts indir=./pipeline_out instrument=FPMA steminputs=nu10012001002 outdir=./products srcregionfile=sourceA.reg bkgregionfile=backgroundA.reg 
```

Optionally, one can run the FTOOLS tool grppha in this step to group the spectrum and flag the bad channels by adding the following parameter to the command line:

```
rungrppha=yes grpmincounts=30 grppibadlow=35 grppibadhigh=1909 
```

Note that the PI range of 35-1909 corresponds to 3-78 keV range which is the best calibrated range for both focal planes. grpmincounts is the minimum counts per bin which you will group your spectrum.  

By the end, you will produce:

Source spectrum
Background spectrum
Source corrected-light curve
Source image
Background-corrected lightcurve
Response matrix file (rmf)
Ancillary response file (arf)


By running grppha,this script also modifies the keywords 'BACKFILE', 'RESPFILE' and 'ANCRFILE' linking them to the corresponding background, rmf and arf files, respectively.  

So ideally, you would want to run 

```
nuproducts indir=./pipeline_out instrument=FPMA steminputs=nu10012001002 outdir=./products srcregionfile=sourceA.reg bkgregionfile=backgroundA.reg rungrppha=yes grpmincounts=30 grppibadlow=35 grppibadhigh=1909  
```

for both FPMA and FPMB.

## Extra Material

Once you grasp the logic behind the data reduction, you can try <code>nustarpipeline</code> module in Python developed by Carlo Ferrigno, ISDC (Univ. Geneva). You can find a simple example Jupyter notebook in the main repository.


