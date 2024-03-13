# LOTUS
Code associated with automated culling process for distinguishing between heliospheric ENA particles and incidental background particles detected by NASA's IBEX satelite. This method is described in Stricklin et al. (2024).

## Introduction
The Interstellar Boundary Explorer (IBEX) satellite collects data on energetic neutral atoms (ENAs) that provide insight into the heliosphere, the region that surrounds our solar system and separates it from interstellar space. IBEX collects information on these particles and on extraneous ''background'' particles. While IBEX records how and when the different particles are observed, it is not able to distinguish between heliospheric ENA particles and incidental background particles. To address this issue, all IBEX data has historically been manually labeled by a subject matter expert (SME) as ''good'' ENA data, or ''bad'' background data. This manual culling process is incredibly time-intensive, and is contingent on subjective, manually-induced decision thresholds. 

This code demonstrates the automated culling process propsed in Stricklin et al. (2024), which serves to supplement the manual culling process, and propose a more standardized data culling approach. Stricklin et al. (2024) presents a three-stage automated culling process, called LOTUS, that makes use of the above data sets, and uses random forests to expedite and standardize the labelling process. In Stage 1, LOTUS uses random forests to obtain probabilities of observing true ENA particles on a per-observation basis. In Stage 2, LOTUS aggregates these probabilities to obtain predictions within small windows of time. In Stage 3, LOTUS refines these predictions to make use of all available information. 

## System Requirements
The code is supported on all operating systems for which the requisite downloads (see below) are possible. The example code was tested on a MacBook Pro running macOS Ventura 13.6.3, using R version 4.3.0.

## Installation 
To downloading and install software and packages:

* R (>= 2.14.0) follow instructions at https://www.r-project.org/

Installation should take less than 15 minutes on a normal desktop computer.

## Demonstration 
The code provided serves to reproduce the figures associated with Stricklin et al. (2024). There are two data files that are considered:
1. Data File #1
2. Data File #2
