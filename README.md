# mlap19
Machine Learning and Application to Physics Workshop '19

This repository is meant to help MLAP19 participants to set up a work environment for the hands-on sessions of the workshop, as well as donwload some example datasets to be used during these sessions. 

It is assumed that there exists a **working [anaconda3](https://www.anaconda.com/distribution/) installation** on the target machine. 

## Installation

Clone the repository:

```console
$ git clone https://github.com/nietootein/mlap19.git
```
Change directory into the cloned repository:

```console
$ cd mlap19
```
Create a new environment:

```console
$ conda env create -f mlap19_env.yml
```
Activate the new environment:

```console
$ conda activate mlap19
```
Verify that the new environment was correctly installed:

```console
$ conda env list
```
The name of the new environment, ```mlap19```, should show up in the list. 

## Updating installation

Should you need to update an already existing ````mlap19``` environment with a newer ```mlap19_env.yml``` version:

```console
$ conda env update --name mlap19 -f mlap19_env.yml --prune
```

## Contact

In case of trouble with the installation please contact Daniel Nieto (d.nieto at ucm dot es) or open an issue [here](https://github.com/nietootein/mlap19/issues).
