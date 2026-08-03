# Shared YHB model tooling

This directory owns native and offline tooling that must not live inside the simulation frontend.

Runtime directories are intentionally ignored by Git:

- `chrono-env/`: Python 3.12 + PyChrono reference solver
- `cadquery-ocp/`: Python 3.12 OCP/VTK target packages
- `micromamba/` and `mamba-root/`: environment bootstrap/cache

Tracked reconstruction inputs:

- `chrono-environment.yml`
- `cadquery-ocp-requirements.txt`
- `fluid-analysis-requirements.txt`

The current frontend reference generator can be run with:

```powershell
$env:YHB_CHRONO_ENV='D:\DEV\Common\yhb-tooling\chrono-env'
D:\DEV\Common\yhb-tooling\chrono-env\python.exe D:\DEV\yhb-simulation-engine\scripts\generate_chrono_reference.py
```

For OCP scripts, use the shared Python 3.12 runtime and expose the target package directory:

```powershell
$env:PYTHONPATH='D:\DEV\Common\yhb-tooling\cadquery-ocp'
D:\DEV\Common\yhb-tooling\chrono-env\python.exe D:\DEV\Aiarwa\scripts\pump-models\inspect-yhb-step-brep.py <input.step> <output.json>
```

The complete original Conda package inventory remains in the simulation repository's `docs/cleanup/CHRONO_CONDA_PACKAGES.csv` and in the pre-cleanup bundle.
