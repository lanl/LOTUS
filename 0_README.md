# LOTUS
Code associated with automated culling process for distinguishing between heliospheric ENA particles and incidental background particles detected by NASA's IBEX satelite. This method is described in Stricklin et al. (2024).

## Introduction
The Interstellar Boundary Explorer (IBEX) satellite collects data on energetic neutral atoms (ENAs) that provide insight into the heliosphere, the region that surrounds our solar system and separates it from interstellar space. IBEX collects information on these particles and on extraneous ''background'' particles. While IBEX records how and when the different particles are observed, it is not able to distinguish between heliospheric ENA particles and incidental background particles. To address this issue, all IBEX data has historically been manually labeled by a subject matter expert (SME) as ''good'' ENA data, or ''bad'' background data. This manual culling process is incredibly time-intensive, and is contingent on subjective, manually-induced decision thresholds. 

This code demonstrates the results of the automated culling process propsed in Stricklin et al. (2024), which serves to supplement the manual culling process, and propose a more standardized data culling approach. Stricklin et al. (2024) presents a three-stage automated culling process, called LOTUS, that makes use of the above data sets, and uses random forests to expedite and standardize the labelling process. In Stage 1, LOTUS uses random forests to obtain probabilities of observing true ENA particles on a per-observation basis. In Stage 2, LOTUS aggregates these probabilities to obtain predictions within small windows of time. In Stage 3, LOTUS refines these predictions to make use of all available information. 

## System Requirements
The code is supported on all operating systems for which the requisite downloads (see below) are possible. The example code was tested on a MacBook Pro running macOS Ventura 13.6.3, using R version 4.3.0.

## Installation 
To downloading and install software and packages:

* R (>= 2.14.0) follow instructions at https://www.r-project.org/

Installation should take less than 15 minutes on a normal desktop computer.

## Demonstration 
Data is provided in **ISOC_Data.zip** and **Orbit_Data.zip**. Details on these data files are provided below. The code provided serves to reproduce the figures associated with Stricklin et al. (2024), and can be found in **LOTUS_Demo.R**.

## Instructions for use
After installing R, run **LOTUS_Demo.R** to reproduce the figures found in Stricklin et al. (2024). Users will need to define the paths at which have saved the two data .zip files on their computer, as well as the path at which they wish to save their plots. This is the only segment in the code that will need to be changed, and can be found at the very top of the code in the call box from lines 14-23.

>[!NOTE]
>The data required to train LOTUS and make subsequent "good" and "bad" label predictions for the various orbits is extremely large (33.2 GB), and so is not included in this repository. Please reach out to the author for additional LOTUS data and code.

## Data Details

### A. Orbit_Data.zip
This .zip file contains two .csv files that are used to make figures that are produced from the labels and probabilities that are produced directly from LOTUS (these correspond to Figures 1 through 5, and all Appendix Figures in Stricklin et al. (2024)).

**Orbit_471.csv** is a data frame that contains the following variables for all ESAs and Angle Bins for _Orbit 471_:
   - _ESA_: 1 of 6 overlapping energy passbands at which "true" ENAs + isotropic background or anisotropic background may be observed
   - _Angle_Bin_: 1 of 60 six-degree bins that inform on the position of IBEX within a given 3-dimensional circular slice of the sky;
   - _Orbit_: An approximately 4.5 day period over which IBEX collects data;
   - _ESA_Sweep_: 1 of 6 overlapping energy passbands at which ENAs + isotropic background or anisotropic backgroundmay be observed (similar variable to ESA, but serves a different purpose for certain plots);
   - _Time_UTC_: Time associated with a given ESA (true or background) observation;
   - _Label_: "True" label assigned by SME (0 refers to a "bad" time (anisotropic background), 1 refers to a "good" time ("true" ENAs + isotropic background);
   - _Counts_: The number of ENAs ("true" + isotropic background or anisotropic background) observed;
   - _L1_Probs_: Probability of a "good" time output by LOTUS Stage 1;
   - _L1_Labs_: Labels output by LOTUS Stage 1;
   - _L2_Probs_: Probability of a "good" time output by LOTUS Stage 2;
   - _L2_Labs_: Labels output by LOTUS Stage 2; 
   - _L2_Probs_SPICE_: Probability of a "good" time output by LOTUS Stage 2, accounting for SPICE spatial positioning data that may have been washed out during LOTUS Stage 2 probability aggregation;
   - _L3_Labs_: Labels output by LOTUS Stage 3; 
   - _L3_Probs_: Probability of a "good" time output by LOTUS Stage 3.

**ENA_Rates.csv** is a data frame that contains the following variables across all ESAs and Angle Bins for _all Orbits_. The variables _ESA_, _Angle_Bin_, _ESA_Sweep_, _Time_, _Label_, and _Counts_ are the same as in Orbit_471.csv.
   - _Orbit_ABs_: An approximately 4.5 day period over which IBEX collects data;
   - _Time_: Time stamp associated with a given ESA (true or background) observation;
   - _Time_SME_: Total exposure time associated with SME-designated "good" labels for a given angle bin;
   - _SME_Rate_: SME ENA rate (total SME-designated "good" labels divided by total exposure time) for a given angle bin;
   - _L1_Rate_: LOTUS Stage 1 ENA rate (total LOTUS Stage 1 "good" labels divided by total exposure time) for a given angle bin;
   - _L3_Rate_: LOTUS Stage 3 ENA rate (total LOTUS Stage 3 "good" labels divided by total exposure time) for a given angle bin;

### B. ISOC_Data.zip
This .zip file contains three .csv files that are used to make figures that are produced from byproducts of the labels that result from LOTUS (these correspond to Figures 6 through 11 in Stricklin et al. (2024)). These files are used for producing ISOC maps for the years 2009 through 2021. **ISOC_SME.csv** is used for producing maps associated with SME-generated labels; **ISOC_L1.csv** is used for producing maps associated with LOTUS Stage 1 labels; **ISOC_L3.csv** is used for producing maps that are associated with LOTUS Stage 3 labels. All files in this .zip file contain the same six variables:
   - _lon_: Longitude associated with observations;
   - _lat_: Latitude associated with observations;
   - _ena_flux_prop_: Transformed version of ENA rate
   - _total_exposure_time_: Total time corresponding to "good" ENA times used to calculate ena_flux_prop;
   - _time_group_: Map associated with observations;
   - _esa_: 1 of 6 overlapping energy passbands at which "true" ENAs + isotropic background or anisotropic background may be observed.

## Attribution and Copyright 
If you use any of the LOTUS framework or results in your work, please cite the following paper:

MA Stricklin, LJ Beesley, BP Weaver, KR Moran, D Osthus, PH Janzen, GD Meadors, and DB Reisenfeld. Moving Towards Automated Interstellar Boundary Explorer Data Selection with LOTUS. Submitted to Statistical Analysis and Data Mining.
#
Copyright 2023 for **CO4627**

This program is Open-Source under the BSD-3 License.   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
