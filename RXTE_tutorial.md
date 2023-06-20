# RXTE-PCA Tutorial

## Some basic information about RXTE-PCA:

RXTE PCA (Proportional Counter Array) consists of 5 PCUs (Proportional Counter Unit) with a total collecting area of 6500 cm2.

Energy range: 2 - 60 keV
Energy resolution: < 18% at 6 keV
Time resolution: 1 microsec
Spatial resolution: collimator with 1 degree FWHM
Detectors: 5 proportional counters
Collecting area: 6500 square cm
Layers: 1 Propane veto; 3 Xenon, each split into two; 1 Xenon veto layer
Sensitivity: 0.1 mCrab
Background: 90 mCrab

You can find more details on analyzing RXTE-PCA data [here](https://heasarc.gsfc.nasa.gov/docs/xte/abc/contents.html).

## Prepare Your Observation:

Now that you have your data downloaded, you will see a structure as follows:

P????? : Proposal level

?????-??-??-?? : Observation level

For each task, you will need to work from the proposal level and run all for each observation separately. Alternatively, you may wish to create your own scripts to work on multiple observations for which you’ll see some examples. 

DO NOT forget to call each list (files with .lis extension) with a “@“!

<code>pcaprepobsid</code> task runs a set of standard RXTE PCA tasks (<code>xtefilt</code>, <code>pcaprepfile2</code>, <code>pcabackest</code>) to prepare your observation before you extract higher-level science products.

- <code>xtefilt</code> creates a filter file that contains the housekeeping information.

- <code>pcaprepfile2</code> estimates background and dead-time values for PCA Standard2 file

- <code>pcabackest</code> creates an estimated PCA background spectrum for a specific observation from the background model created by the PCA instrument team

You can start the task by running:

```
pcaprepobsid
```

Which will prompt you to enter:

<code>Name of input observation ID directory [ ] $obsid$</code>

<code>Name of output results directory [] $obsid$-result</code>

Alternatively, you can use a single command line:

```
pcaprepobsid indir="$obsid$" outdir="$obsid$-result"
```

In case you want to process a set of observations, you can easily create a script for pcaprepobsid as follows:

```
#!/bin/bash

obslist=10411-??-??-??
for obsid in $obslist; do
    pcaprepobsid indir=${obsid} outdir=${obsid}-result
  done
```
You can repeat it for as many observations as you like and run the file (e.g. pcaprepobsid.sh):

```
/.pcaprepobsid.sh
```

Make sure you are working in a directory you have writing privileges and try to avoid working on an external drive.

### Merging Observations:

In case you want to work with science products that are obtained from not just one but a set of observations, you can merge these observations once you process each one. This is done first by creating a list of observations you want to merge with:

```
ls -d $propid$*-result > $propid$-all.lis
```
And then run <code>pcamergeobsids</code> in one line:

```
pcamergeobsids indir=@$propid$-all.lis outdir=$propid$-all-result
```

The method above assumes that you are working within one Proposal level with proposal id <code>$propid$</code>, let's say 91702. As in the case with <code>pcaprepobsid</code>, you will be running the command within the directory you have all these observation directories in.

## Create a Good Time Interval (GTI) File:

In this step, before you produce higher-level science products you need to filter out bad data from your observations based on certain screening criteria. It’s advised to apply basic screening criteria on each spectrum. This includes removing data from when the target is below the earth horizon (ELV cut), and or when RXTE is not pointed at the desired target, or when PCU detectors are off. 

You can start the task by running:

```
maketime 
```

Or you can use a one-line command as follows:

```
maketime infile=$obsid$-result/*.xfl outfile=$obsid$.gti expr="$expr" value=VALUE time=TIME prefr=0.5 postfr=0.5 compact=NO clobber=YES
```

Here, <code>$expr</code> contains all of the screening criteria you wish to apply. For our case this will be:

```
(ELV > 4) && (OFFSET < 0.1) && (NUM_PCU_ON.EQ.2) && .NOT. ISNULL(ELV)
```

Where we also select data obtained only by PCU2 which was operated almost always on and is best calibrated.

## Extract a Background Corrected Spectrum and Lightcurve:

Now that you have everything you need ready for your science products, you can start by extracting a background-corrected spectrum and lightcurve by running pcaextspect2 and pcaextlc2, respectively.

### Spectrum:

```
pcaextspect2
```

With the following parameters

```
src_infile=@$obsid$-result/FP_dtstd2.lis
bkg_infile=@$obsid$-result/FP_dtbkg2.lis
src_phafile=$obsid$_src.pha 
bkg_phafile=$obsid$_bkg.pha
gtiandfile=$obsid$.gti
filtfile=@$obsid$-result/FP_xtefilt.lis
respfile=$obsid$.rsp
pculist=2 
layerlist=ALL
```

Alternatively,

```
pcaextspect2  src_infile=@$obsid$-result/FP_dtstd2.lis  bkg_infile=@$obsid$-result/FP_dtbkg2.lis  filtfile=@$obsid$-result/FP_xtefilt.lis  src_phafile=$obsid$.fits bkg_phafile=$obsid$_bkg.fits gtiandfile=$obsid$.gti respfile=$obsid$.rmf pculist=2 layerlist=ALL
```


### Lightcurve: 

```
pcaextlc2 
```

With the following parameters

```
src_infile=@$obsid$-result/FP_dtstd2.lis
bkg_infile=@$obsid$-result/FP_dtbkg2.lis
outfile=$obsid$.lc
gtiandfile=$obsid$.gti
pculist=2 
layerlist=ALL 
binsz=16
```

Alternatively, 

```
pcaextlc2 src_infile=@$obsid$-result/FP_dtstd2.lis bkg_infile=@$obsid$-result/FP_dtbkg2.lis outfile=$obsid$.lc gtiandfile=$obsid$.gti pculist=2 layerlist=ALL binsz=16
```
Et voilà! You have your spectrum and lightcurve.

## Bin Spectrum and Account for Instrumental Uncertainties.

As any other instrument, RXTE-PCA is also subject to uncertainties that are specific to the characteristics of the instrument. Previously, PCA team recommended to apply 1% of uncertainties to your spectrum, this is done by running FTOOLS tool <code>grppha</code>:

You can run <code>grppha</code> from your terminal or alternatively you can run a single line command:

```
grppha infile.fits outfile.fits 'group min 25 && systematics 0-128 0.01 & exit'
```

Where <code>infile.fits</code> is the spectrum you created above and <code>outfile.fits</code> is going to be the name for your binned spectrum.

To better improve the calibration of your spectrum, you can use [<code>pcacorr</code>](https://sites.srl.caltech.edu/~javier/crabcorr/index.html). See the [paper](https://ui.adsabs.harvard.edu/abs/2014ApJ...794...73G/abstract) for more information.

Download the latest version of the code [here](https://drive.google.com/file/d/1pR3weteidD_8Ag9aNR56RFxuc7OZfJVH/view?usp=sharing).

run <code>pcacorr.py</code> for your spectrum with the background:

```
python3.9 pcacorr.py -b spectrum.fits
```

WARNING: This is for epoch gains 4-5, please check other options to process spectra obtained during different epoch gains.

This applies an empirical correction to your spectrum. Instead of 1% systematic uncertainty, you can now reduce it to 0.1% and bin your spectrum:

```
grppha infile.fits outfile.fits 'group min 25 && systematics 0-128 0.001 & exit'
```

## Extra material:

The above-mentioned steps will create a time-averaged spectrum for your observation. This means that your products are going to be averaged throughout your entire observation (or observations if you choose to work with multiple observations). However, you might be interested in analyzing a certain part of your observation i.e. a specific burst or you want to produce a spectrum that excludes a certain time interval. You can do this by selecting certain time intervals of your interest from the lightcurve you extracted.

Start by displaying your spectrum:

```
lcurve 1 filename.lc
```
If you need to, zoom in on the time interval by rescaling your axes:

```
r x low high
```

and 

```
r y low high
```

Note down your time intervals as follows:

start_time end_time for your first phase and the second phase separately. 

Note that these time values you obtain from your lightcurve are relative to the <code>TSTART</code> and you need to transfer this time to absolute time to be used later on. This task is carried out by running the tool <code>timetrans</code>. You can find the value of <code>TSTART</code> from the HEADER of your lightcurve. You can do so by using <code>fv</code> tool in HEASOFT:

```
fv filename.fits
```
Similar to the time-averaged spectrum, you will need to have created your GTI file which will be [AND'ed](https://heasarc.gsfc.nasa.gov/docs/xte/abc/extracting.html#array_ex1_gtiandfile). 


Below is an example of running <code>SAEXTRACT</code>

For full documentation about saextrct, type fhelp saextrct.

The corresponding screen dialogue is as follows:

Run:

```
saextract
```

Below are parameters and suggested values to extract your products:


Input file name or @file-of-filenames:[] <code>@std2.list</code>

Input GTI files to be OR'd with INFILE (APPLY):[APPLY]

Input GTI file to be AND'd with INFILE:[-] <code>elv_gt_10.gti</code>

Root name for output file:[sac] pca_32s

Accumulate (ONE) or (MANY) Spectral/Light Curves:[ONE]

Name of TIME column:[TIME]

Name of COLUMNS to be accumulated (GOOD):[GOOD]

Input the binsize in seconds, use 0.1 etc. if nec (INDEF):[INDEF] 

Minimum acceptable fractional exposure (INDEF):[INDEF]

Chose print option, LIGHTCURVE, SPECTRUM, or BOTH:[BOTH] <code>BOTH</code> or change it to your preference.

Type of binning for LIGHTCURVE: (SUM, RATE, MEAN):[RATE]

Type of binning for SPECTRUM (SUM, RATE, MEAN):[SUM]

Maximum acceptable intensity for Light Curve (INDEF):[INDEF]

Maximum acceptable intensity for Spectrum (INDEF):[INDEF]

Column titles for SUM written as header Keywords:[-]

Column titles for MEAN written as header Keywords:[-]

Starting time for summation (INDEF):[INDEF]

Ending time for summation (INDEF):[INDEF]

Input time intervals t1-t2,t3-t4 in seconds (INDEF): <code>time intervals</code> as you obtained from <code>timetrans</code>

Minimum energy bin to include in Spectra (INDEF):[INDEF]

Maximum energy bin to include in Spectra (INDEF):[INDEF]

Input channels to be retained 1-2,3-4 (INDEF):[INDEF]

Input channels for each bin 1-5,6-256 (INDEF):[INDEF]

Perform a dryrun on files without processing any data? (Yes):[no]
