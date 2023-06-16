# XMM-Newton Tutorial

## Download your Data:

As discussed previously, you can search and download your data through HEASARC, but for XMM-Newton it’s better to download your data directly from XMM-Newton’s science archive http://nxsa.esac.esa.int/nxsa-web/#search 

Start by searching and finding your source:

<p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/b0879373-d2a9-484c-84b6-f4a2c5f065b3">
</p>

Click on the green download symbol next to the observation ID you want to download:

  Download your observation data file or ODF.

<p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/3c5afb06-ef72-4db9-bbb5-1547679661c7">
</p>

  Alternatively, you can add your selected observations to your basket and download them as a compressed folder after you log in. You will need to create an account and receive an email when your files are ready for download:
  
  <p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/f12d0ad4-00c8-430e-a973-392084f83bd5">
  </p>
  
  
## Data Reduction- XMM-Newton EPIC - imaging mode:

The European Photon Imaging Camera or EPIC consists of three CCD cameras onboard XMM-Newton: Two Metal Oxide Semi-conductor or MOS (MOS1 and MOS2) and PN. MOS cameras are located behind the Reflection Grating Spectrometers (RGS). The gratings divert about half of the telescope incident flux towards the RGS detectors such that (taking structural obscuration into account) about 44% of the original incoming flux reaches the MOS cameras. This is not the case for PN which receives and unobstructed beam.

Energy range: 0.15-15.0 keV

Field of view: 30 arcmin

Angular resolution: PSF, 6 arcsec FWHM

Operating Modes:

#### Full frame and Extended full frame (PN only): 
In this mode, all pixels of all CCDs are read out and thus the full FoV is covered

#### Partial Window:

- MOS

In a partial window mode the central CCD of both MOS cameras can be operated in a different mode of science data acquisition, reading out only part of the CCD chip: in small window mode an area of 100 x 100 pixels is read out, whereas in large window mode an area of 300 x 300 pixels is active.  - PN In large window mode only half the area of all 12 CCDs is read out, whereas in small window mode only the part of CCD0 in quadrant 1 (CCD4 according to SAS numbering conventions) at the focal point is used to collect data

#### Timing Mode:

- MOS + PN

In timing mode, imaging is made only in one dimension, along the column axis. Along the row direction, data from a predefined area on one CCD chip are collapsed into a one-dimensional row to be read out at high speed - PN only A special flavour of the timing mode of the EPIC pn camera is the burst mode, which offers very high time resolution, but has a low duty cycle of 3%

For a more detailed information on the instrument specifics, please visit: 
https://www.cosmos.esa.int/web/xmm-newton/technical-details-epic#PN

The full list of science threads: https://www.cosmos.esa.int/web/xmm-newton/sas-threads


### Prepare Your Data:

WARNING: Each step described below assumes your observation is not affected by pile-up. More discussion and how to deal with it is at the end of this tutorial.

- Initialize HEASOFT and SAS:

```
heainit
```

```
sas
```
- Define ODF paths in your bashrc/zshrc:

```
export ODF_PATH=/PATH/TO/YOUR/ODF/

export SAS_CCF=$ODF_PATH/

export SAS_ODF=$ODF_PATH/
```

Make sure you are working in your <code>ODF_PATH</code>!


- Create a calibration index file or CIF:

```
cifbuild
```

This task identifies all of the CCF files to process your observation data and creates a <code>ccf.cif</code> in under <code>ODF_PATH</code>. Once ccf.cif is created, add it to <code>SAS_CCF</code>

```
export SAS_CCF=$ODF_PATH/ccf.cif
```

- Create a summary file *SUM.SAS that include all of the information about your observation:

```
odfingest 
```

Once <code>*SUM.SAS</code> is created, add it to the end of <code>SAS_ODF</code>:

```
export SAS_ODF=$ODF_PATH/*SUM.SAS
```

- Now run EPIC specific data reduction tasks: 
  
For MOS:
  
```
emproc
```
  
  For PN:

```
epproc
```

  Each task will reprocess your event files for further processing. Each will take a little while depending on your machine, take a short break if your task is running successfully.


- Once your event files are created, check the contents of your directory:

```
ls -ltr
```

You will see your events file created for each observing mode for each camera. For a coherent name structur, you might want to choose to rename your files as follows:

```
mv original_nameMOS1.evt MOS1_evt.fits

mv original_nameMOS2.evt MOS2_evt.fits

mv original_namePN.evt PN_evt.fits
```

Your data are now ready for further processing!

### Filter Event Files for a Flaring Background:

WARNING: For the following steps, <code>EPIC</code> is going to be the name of your camera name, i.e. MOS1, MOS2 or PN. You will run each task for each event file available for each camera. Make sure you are changing the names for each step!

- Extract a light curve from the event file to identify intervals of flaring particle background:  

```
evselect table=EPIC_evt.fits withrateset=Y rateset=rateEPIC.fits maketimecolumn=Y timebinsize=100 makeratecolumn=Y expression='Selection_Expression'
```

where Selection_Expression is:

<code>”#XMMEA_EM && (PI>10000) && (PATTERN==0)”</code> for EPIC-MOS

<code>”#XMMEA_EP && (PI>10000&&PI<12000) && (PATTERN==0)”</code> for EPIC-PN

 - Check your lightcurve for flaring background:
 
 ```
 dsplot table=rateEPIC.fits x=TIME y=RATE.ERROR
 ```
  
<p align="center">
  <img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/94107527-df2e-4cbc-9240-1587d122a34b">
</p>
  
- Determine where the light curve is low and steady. Choose a threshold (count/second) just above the low steady background to define the "low background" intervals, to create the corresponding GTI file:

```
tabgtigen table=rateEPIC.fits expression='Rate_Expression' gtiset=EPIC.gti
```
  
where <code>Rate_Expression</code> is
	
For EPIC-MOS:
	
```
RATE<=0.35
```
For EPIC-PN:
					
```
RATE<=0.4
```	

This value will be different for each data set (the value given here for each instrument represents a good reference value for a standard observation).
 
- Now create the filtered event files:

```	
evselect table=EPIC_evt.fits withfilteredset=Y filteredset=EPICclean.fits destruct=Y keepfilteroutput=T expression='Selection_Expression'	
```	

where <code>Selection_Expression</code> is
	
For EPIC-MOS:
	
```
#XMMEA_EM && gti(EPICgti.fits,TIME) && (PI>150)
```
For EPIC-PN:
					
```
#XMMEA_EP && gti(EPICgti.fits,TIME) && (PI>150)
```	
  
The file <code>EPICclean.fits</code> contains the filtered EPIC event list and can be now used to produce scientific products. The file <code>EPICgti.fits</code> contains the definition of Good Time Intervals.

Be aware: if you are interested in very short time periods, such as they appear in pulsars or cataclysmic variables, you have to perform a barycentric correction. This means that the arrival time of a photon is shifted as is it would have been detected at the barycentre of the solar system (the centre of mass) instead at the position of the satellite. In this way, the data are comparable. The SAS task barycen performs this correction. As barycen overwrites the TIME column entries, it is advisable first to copy the original event list and keep it as a backup.

```
cp PNclean.fits EPICclean_nobarycen_cor.fits
barycen table=PNclean.fits:EVENTS
```

### Extract an Image:
	
- Extract an image in detector coordinates:

```
evselect table=EPICclean.fits imagebinning=binSize imageset=EPICimage.fits withimageset=yes xcolumn=X ycolumn=Y ximagebinsize=80 yimagebinsize=80
```

- Display your image by running:

```
imgdisplay withimagefile=true imagefile=EPICimage.fits
```

or 

```
ds9 EPICimage.fits
```

A ds9 window will appear:

<p align="center">
	<img width="625" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/e9807511-af02-41fe-b43e-0015e6955c3a">
</p>

### Extract a Lightcurve:

- From your displayed image, select a region of your source and background, note these region coordinates and size (in the following threads, you will be using the physical size and coordinates):
	
	<p align="center">
	<img width="622" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/c7d8d650-6760-463c-a97e-da84e7331ca8">
	</p>

- Extract your lightcurve for your source region:
	
```
evselect table=EPIC_evt.fits energycolumn=PI expression='Selection_Expression' withrateset=yes rateset="EPIC_source_lightcurve_raw.lc" timebinsize=10 maketimecolumn=yes makeratecolumn=yes
```	

where <code>Selection_Expression</code> is
	
For EPIC-MOS:
	
```
#XMMEA_EM && (PATTERN<=12) && (PI in [200:10000])
```
For EPIC-PN:
					
```
#XMMEA_EP && (PATTERN<=4) && (PI in [200:10000])
```	

- Repeat the above step for your background to create <code>EPIC_bkg_lightcurve_raw.lc</code>

- Now correct your light curve for effects such as vignetting, bad pixels, PSF variation and quantum efficiency, as well as for variations affecting the stability of the detection within the exposure, like dead time and GTIs

```
epiclccorr srctslist=EPIC_source_lightcurve_raw.lc eventlist=EPICclean.fits outset=EPIC_lccorr.lc bkgtslist= EPIC_bkg_lightcurve_raw.lc withbkgset=yes applyabsolutecorrections=yes
```

This step produces your final background-corrected lightcurve.

Display your lightucurve by running:

```
dsplot table=EPIC_lccorr.lc withx=yes x=TIME withy=yes y=RATE
```

Or 

```
lcurve 1 EPIC_lccorr.lc 
```

which prompts an FTOOLS tool lcurve for further analysis of your lightcurve.

### Extract MOS Spectra:

- Using the same source and background regions, you can start the extraction of the MOS spectra by running for both MOS1 and MOS2 separately.
	
First extract the source spectrum by running:	
	
```
evselect table=MOSclean.fits withspectrumset=yes spectrumset=MOSsource_spectrum.fits energycolumn=PI spectralbinsize=5 withspecranges=yes specchannelmin=0 specchannelmax=11999 expression='#XMMEA_EM && (PATTERN<=12) && ((X,Y) IN circle(30360.5,28400.5,640))'
```
	
Now the background:

```
evselect table=MOSclean.fits withspectrumset=yes spectrumset=MOSbackground_spectrum.fits energycolumn=PI spectralbinsize=5 withspecranges=yes specchannelmin=0 specchannelmax=11999 expression='#XMMEA_EM && (PATTERN<=12) && ((X,Y) IN circle(30720.5,26360.5,640))'
```

- Calculate the area of your source and background:
	
```
backscale spectrumset=MOSsource_spectrum.fits badpixlocation=MOSclean.fits
```
	
```
backscale spectrumset=MOSbackground_spectrum.fits badpixlocation=MOSclean.fits
```
	
- Create a redistribution matrix for your previously extracted spectrum:
	
```
rmfgen spectrumset=MOSsource_spectrum.fits rmfset=MOS.rmf
```

This step too can take a while, take a break!
	
	
- Once rmfgen is done running, generate your ancillary file:
	
```
arfgen spectrumset=MOSsource_spectrum.fits arfset=MOS.arf withrmfset=yes rmfset=MOS.rmf badpixlocation=MOSclean.fits detmaptype=psf
```

- Now group your spectrum and rebin:
	
```
specgroup spectrumset=MOSsource_spectrum.fits mincounts=25 oversample=3 rmfset=MOS.rmf arfset=MOS.arf backgndset=MOSbackground_spectrum.fits groupedset=MOS_spectrum_grp.fits
```
	
<code>mincounts=25</code> is the widely accepted minimum value, you might change it depending on your spectrum or you can use external algorithms that search for the optimal binning for your data. 

### Extract PN Spectrum:
	
- Repeat the steps above for EPIC-PN spectrum only by changing: 

```
specchannelmin=0 specchannelmax=20479 expression='(FLAG==0) && (PATTERN<=4) && ((X,Y) IN circle(30360.5,28400.5,640))'
```

Check the Xspec tutorial for Spectral Analysis!
		
### Observations Taken in Timing Mode:

In some instances, your source might be too bright for the CCD detectors that photons arriving at the same time can be read as one event (i.e. one photon but with the total energy of the two/more arriving at the same time). This is called “pile-up” and unfortunately, many X-ray binaries experience this issue. To overcome this, some observations are carried out in Timing mode in which only one CCD chip operates and data are read out at high speed in one dimension row (RAWX) instead of imaging mode.

It’s preferable to use PN as the active CCD is operated at full width while this is not the case for MOS where the number of columns around the foresight is reduced to 100.

Below, we will reduce EPIC-PN data obtained in timing mode.

As we’ve done in the previous example with the imaging mode observations, you will go through the same steps until you get your cleaned events. 

Unless your timing mode is BURST, this thread picks up from the point where you are about to extract your image. If it was taken in the burst mode, run <code>epproc</code>:

```
epproc burst=yes
```

Otherwise, the prerequisite tasks are the same and you can pick up from extracting your image:

```
evselect table=PNclean.fits imagebinning=binSize imageset=PNimage.fits withimageset=yes xcolumn=RAWX ycolumn=TIME ximagebinsize=1 yimagebinsize=1
```

Note that the command line is different from the imaging mode. You have different column inputs for X and Y columns!

Display your image:

```
ds9 PNimage.fits
```
<p align="center">
	<img width="500" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/c4f8dd68-e724-4ed4-a523-f649a7eb8468">
</p>		

You'll notice that the spatial information is encoded in one dimension RAWX instead of two dimensional image we extracted for the imaging mode!

Depending on the brightness of your source, you will select columns instead of a circular region. 

- This is done a little differently than in the imaging mode:
																																					
```
evselect table=PNclean.fits withspectrumset=yes spectrumset=PNsource_spectrum.fits energycolumn=PI spectralbinsize=5 withspecranges=yes specchannelmin=0 specchannelmax=20479 expression='(FLAG==0) && (PATTERN<=4) && (RAWX>=32) && (RAWX<=44)'
```

<code>(RAWX>=35) && (RAWX<=41)</code> selects the 5 columns between the 35th and 41st columns, centred around 38 (35 and 41 not included). Note that this region selection is just an example, it will depend on your observation!
	
- Now the background:

```
evselect table=PNclean.fits withspectrumset=yes spectrumset=PNbackground_spectrum.fits energycolumn=PI spectralbinsize=5 withspecranges=yes specchannelmin=0 specchannelmax=20479 expression='(FLAG==0) && (PATTERN<=4) && (RAWX>=3) && (RAWX<=5)'
```
	
The rest of the extraction of the spectrum is done the same way. Repeat the steps above. Check the Xspec tutorial for the spectral analysis example.
	
### Pile-Up and How to Deal with It?
	
As mentioned above, many bright X-ray sources are subject to the effects of pileup. Even when you try to compensate for it by choosing a timing mode for your observation, it’s still good to check if your observation is affected by pile-up. 

The way to do so is pretty simple. First, you need to obtain your cleaned events as described early in this tutorial. Then, you will obtain a pattern plot which shows the distribution of your single and double events in comparison to the modelled distribution for your observation.

- First, filter your clean event file only to include your source selection region:

```
evselect table=PNclean.fits withfilteredset=yes filteredset=PN_filtered.evt keepfilteroutput=yes expression="(RAWX>=35) && (RAWX<=41) && gti(PN.gti,TIME)"
```
	
- Now, extract the pattern distribution:

```
epatplot set=PN_filtered.evt plotfile=“PN_filtered_pat.ps"
```

- Display your plot:
	<p align="center">
	<img width="622" alt="image" src="https://github.com/anastasiyayilmaz/X-ray-Data-Processing-Workshop/assets/57295156/c2fc04ef-4371-4d5b-9d79-8c884336550f">
	</p>

As you can see, the registered double events are higher than what the model predicts. This is a sign of pile-up. 

Independent of whether you are working with timing or imaging mode, they method relies on removing the brightest parts you see in the image starting from the innermost region. For timing mode, it will be either the central column or the central three and for imaging mode, it might correspond to >90% of your source region. Let’s continue with the timing mode for now, please try the imaging mode yourself. One nice example if you want to try is LMC X-3 which is severely piled up in EPIC-imaging observations.

- First, let’s try selecting removing only the central column:

```
evselect table=PNclean.fits withfilteredset=yes filteredset=PN_filtered.evt keepfilteroutput=yes expression="( RAWX in [35:38] || RAWX in [38:41] )"
```

- Now, extract your pattern distribution and see if the observed distribution is nearly the same as the modelled distribution. If not, try the next two columns and repeat the steps above. Normally, the first three columns should be enough for timing mode. Be aware, the more columns you remove the more flux you will lose.

- Now that you made sure you no longer have the effects of pile-up on your distribution plot, you can move on with extracting your background-corrected spectrum as described above. 

### Extra Material:
	
Once you grasp the logic behind the data reduction with SAS, you can use pyxmmsas module in python developed by Carlo Ferrigno, ISDC (Univ. Geneva).

You can take a look at a detailed example Jupyter notebook: https://gitlab.astro.unige.ch/ferrigno/4u-1907-xmm/-/blob/3b2e7b905c5d5bc8049437b4494a4b46279f2531/Phase-Averaged_Analysis.ipynb 
	
You can install it using pip:

```
pip install pyxmmsas
```

Or
	
```
python3.9 -m pip install pyxmmsas
```
