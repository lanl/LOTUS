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
The code provided serves to reproduce the figures associated with Stricklin et al. (2024). There are two data sources that are considered:
1. Orbit_Data.zip
2. ISOC_Data.zip

#
Copyright 2023 for CO4627

This program is Open-Source under the BSD-3 License.   Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

