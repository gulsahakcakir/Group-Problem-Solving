# Copy or Collaborate? How Networks Impact Collective Problem Solving

Code and data for:

> Akçakır, G., Lang, J. C. & Lamberson, P. J. Copy or collaborate? How networks impact collective problem solving. *npj Complexity* **2**, 35 (2025). https://doi.org/10.1038/s44260-025-00058-8

This repository contains an agent-based model of teams searching a fitness landscape. In each time step, every agent either **copies** (looks at its network neighbours' current solutions) or **collaborates** (searches nearby solutions using its neighbours' skill types), and moves to the best payoff it can see. The scripts reproduce the simulation results and Figures 3–7 of the paper.

## Repository layout

```
scripts/
  setup.R                    shared setup: libraries, seed, cores, distance matrix
  simulation_func.R          simulate(): the agent-based model
  MW_landscape.R             complex landscape (Mason & Watts, as implemented by Barkoczi & Galesic)
  networks.R                 team networks (linear, fully connected, linear + random edges)
  distances.R                pairwise distances between grid cells
  combine_results.R          combines result batches from data/
  simulate_overTime.R        Figures 3 and 4
  simulate_netInteraction.R  Figure 5
  simulate_density.R         Figure 6
  simulate_colFirst.R        Figure 7
  figures.R                  builds Figures 3–7 from data/
data/                        simulation results (.RData, one file per batch of 50 replications)
figures/                     figure PDFs
```

## Requirements

R (tested with 4.3.2) and the following packages:

| Package    | Tested version |
|------------|----------------|
| tidyverse  | 2.0.0          |
| igraph     | 2.1.2          |
| akima      | 0.6.3.4        |
| patchwork  | 1.2.0          |
| zoo        | 1.8.12         |

```r
install.packages(c("tidyverse", "igraph", "akima", "patchwork", "zoo"))
```

## Reproducing the figures

Run all commands from the repository root.

```bash
Rscript scripts/figures.R
```

This reads the result files in `data/` and writes the PDFs to `figures/`:

| Figure | Script that generates the data | Data files in `data/` | Output |
|--------|-------------------------------|-----------------------|--------|
| 3, 4 | `simulate_overTime.R` | `overTime_r6_types100_timeLimitFALSE1_*.RData` | `overTimeMax.pdf`, `overTimeAve.pdf` |
| 5 | `simulate_netInteraction.R` | `networkInteraction_r6_types100_timeLimitFALSE1_*.RData` | `networkInteraction.pdf` |
| 6 | `simulate_density.R` | `density_r6_types100_timeLimitFALSE1_*.RData` | `density.pdf` |
| 7 | `simulate_colFirst.R` | `collaborateFirst_r6_types100_timeLimitTRUE10_*.RData` | `collabFirst.pdf` |

If the data for a figure is not in `data/`, `figures.R` skips that figure and prints a message.

## Rerunning the simulations

Each `simulate_*.R` script runs one batch of replications for every parameter combination and saves it to `data/`.

```bash
Rscript scripts/simulate_netInteraction.R
```

The results in `data/` were produced on an SGE cluster as independent jobs of 50 replications each (for example, 20 jobs for 1,000 replications). The scripts read these environment variables:

| Variable | Meaning | Default |
|----------|---------|---------|
| `N_REPS` | replications per job | `50` |
| `NSLOTS` | cores used by `mclapply` (set by SGE) | `1` |
| `JOB_ID` | added to the output file name (set by SGE) | empty |
| `SEED`   | master random seed | `JOB_ID`, or `2025` when running locally |

Runs with the same `SEED` and `N_REPS` produce identical results. Every replication also stores its own seed in the `rand_seed` column. A quick local test:

```bash
N_REPS=1 Rscript scripts/simulate_colFirst.R
```

A full batch is slow; the cluster runs used several cores per job.

## Citation

If you use this code, please cite the paper above. Citation metadata is in [`CITATION.cff`](CITATION.cff).
