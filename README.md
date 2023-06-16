# X-ray Data Processing Workshop, Silesian University Opava (19-21 June 2023)

A set of extra material intended for the X-ray Data Processing Workshop. Includes the following:

- Instructions on how to find and download data from HEASARC's archive.
- Instructions on how to reduce XMM-Newton, RXTE and NuSTAR data and obtain science products (separate tutorials for each)
- A Jupyter notebook using [PyXspec](https://heasarc.gsfc.nasa.gov/xanadu/xspec/python/html/index.html) for spectral analysis
- A Jupyter notebook to produce science products from NuSTAR FPMA and FPMB using [nustarpipeline](https://gitlab.astro.unige.ch/ferrigno/nustar-pipeline), developed by Carlo Ferrigno (ISDC, University of Geneva).
- An Xspec Tutorial for a simple spectral analysis.

Both notebooks assume you have successfully installed [HEASOFT](https://heasarc.gsfc.nasa.gov/docs/software/lheasoft/) and have your [CALDB](https://heasarc.gsfc.nasa.gov/docs/heasarc/caldb/caldb_intro.html) set up. To be able to use PyXspec, make sure you defined your Python path before installing HEASOFT.

## HEASOFT Installation: 

1- Go to https://heasarc.gsfc.nasa.gov/docs/software/lheasoft/

2- Download the file for your system.

3- Untar the file once it’s downloaded.

4- Check the system requirements and follow the steps before installing the software

Please check the known issues page (https://heasarc.gsfc.nasa.gov/docs/software/lheasoft/ issues.html) for any common issues users encounter.

## SAS Installation:

1- After successfully installing HEASOFT, go to https://www.cosmos.esa.int/web/xmm-newton/ download-and-install-sas

2- Download the package for your system.

3- Make sure you meet the system requirements. 4- Follow the instructions and install SAS.

## Working with Calibration Database CALDB:

NuSTAR and RXTE calibration files are accessed through HEASOFT while XMM-Newton has its own database. Please follow the steps below to make sure you have access to the most up-to-date calibration files for the data reduction.

### HEASOFT:

HEASOFT offers the option to access the CALDB files remotely additional to downloading them on your machine. The first is rather better since these files get updated frequently for each instrument.

Instructions for remote access:

The setup is fairly easy and can be followed from : https://heasarc.gsfc.nasa.gov/docs/heasarc/ caldb/caldb_remote_access.html

One important thing to keep in mind is that you will need to make sure to have stable internet connection throughout your data reduction no matter which instrument you are working with.

### SAS:

XMM-Newton SAS has a different set of CALDB files which can be mirrored to your repository following either options here: https://www.cosmos.esa.int/web/xmm-newton/current-calibration-files

## Defining PATHS:

For both HEASOFT and SAS, please make sure the require paths are defined in your bashrc/zshrc as described in the instructions above. Below’s an example of what you will need, but beware that these paths will be different for your system. Please check the instructions specific for your operating system:

### Prerequisites:

```
export CC=/usr/bin/clang
export CXX=/usr/bin/clang++
export PERL=/usr/bin/perl
export FC=/opt/homebrew/bin/gfortran-12
export PYTHON=/opt/homebrew/bin/python3.9
```
            
### HEASOFT:
```
export HEADAS=/Users/anastasiyayilmaz/Documents/soft/heasoft-6.31.1/aarch64-apple-darwin22.2.0
source $HEADAS/headas-init.sh
alias heainit=". $HEADAS/headas-init.sh"
export PATH="$HEADAS/bin:$PATH"
export PATH="/usr/bin:$PATH"
export PYTHONPATH=$HEADAS/lib/python:$HEADAS/lib
export DYLD_LIBRARY_PATH=$HEADAS/lib
```

### CALDB:

```
CALDBCONFIG=/Users/anastasiyayilmaz/Documents/soft/CALDB/software/tools/caldb.config
export CALDBCONFIG
CALDBALIAS=/Users/anastasiyayilmaz/Documents/soft/CALDB/software/tools/alias_config.fits
export CALDBALIAS
CALDB=https://heasarc.gsfc.nasa.gov/FTP/caldb
export CALDB
```

### SAS:
```
export SAS_DIR=/Users/anastasiyayilmaz/Documents/soft/sas_19.1.0-macOS-10.15.7/xmmsas_20210317_1624
alias sas="source $SAS_DIR/setsas.sh"
export SAS_PERL=/usr/bin/perl
export ODF_PATH=/Users/anastasiyayilmaz/Documents/data/GRS1915/ayilmaz12973805/0112990501
export SAS_CCF=$ODF_PATH/ccf.cif
export SAS_ODF=$ODF_PATH/0706_0112990501_SCX00000SUM.SAS
export SAS_CCFPATH=/Users/anastasiyayilmaz/Documents/soft/CCF/
export PYTHONPATH=/usr/bin/python3
```

Initialize HEASOFT:

```
heainit
```

Initialize SAS:

```
sas
```

#### How to install <code>nustarpipeline</code>:

```
pip install nustarpipeline
```

or
```
python3.9 -m pip install nustarpipeline
```

#### How to install <code>pyxmmsas</code>:

```
pip install pyxmmsas
```

or
```
python3.9 -m pip install pyxmmsas
```

For Jupyter notebooks, check if you have <code>PyXspec</code>, <code>nustarpipeline</code> and <code>pyxmmsas</code> working:


```
python3.9 -c 'import xspec'
```

```
python3.9 -c 'import nustarpipeline'
```

```
python3.9 -c 'import pyxmmsas'
```

If you receive no message, you are all set!


You can run the <code>nustarpipeline</code> threads from the notebook or use process_nustar.py. 

For usage and detailed options, run:

```
process_nustar.py
```




