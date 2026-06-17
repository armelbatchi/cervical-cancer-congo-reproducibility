# GitHub to Zenodo steps

1. Create a public GitHub repository.
2. Upload all files from this package to the repository.
3. Commit the files with the message: `Initial reproducibility package`.
4. Confirm that the repository contains no private files.
5. Log in to Zenodo and connect the GitHub account.
6. In Zenodo, enable this GitHub repository.
7. On GitHub, create a new release with tag `v1.0.0`.
8. Wait for Zenodo to archive the release and mint a DOI.
9. Copy the Zenodo DOI.
10. Update `CITATION.cff`, `metadata/.zenodo.json`, `README.md`, and the manuscript data availability statement with the DOI.
11. Commit the DOI update to GitHub.
12. If needed, make a second release, `v1.0.1`, for the DOI-updated repository.
