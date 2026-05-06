# How Hard is it to Rig a Benchmark? A Social Choice Analysis of Leaderboard Robustness
This repository contains the code to reproduce all experiments in our paper titled "How Hard is it to Rig a Benchmark? A Social Choice Analysis of Leaderboard Robustness". 

You need access to the HELM MMLU raw run folders (v1.0.0). There are two ways to obtain them:

Option A: Download the HELM MMLU raw results under this link: https://zenodo.org/records/18402602?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjI1MDc3YWZjLWIzYWEtNDUzYy05NzBkLTY4OTA5NmEwMjcwOSIsImRhdGEiOnt9LCJyYW5kb20iOiI4NmI4MjY1MWJkMWNiZTFmNzM5NDFiYmUyYTc2YTI0MiJ9.SfRSP6FsVUywpMI0iBcdhiRran2YRlGyeso2JPgoPxhl7KXCWtCiMcziKANBDr6V2zUqQUBR7WeeMhEdjGodAQ. 

Option B: Download the HELM MMLU raw results from the public crfm-helm-public bucket in the Google Cloud Storage (GCS) by completing the following two steps:

1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions: 
https://docs.cloud.google.com/sdk/docs/install-sdk.

2.	Download HELM MMLU raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/.

After downloading via Option A or B, the code expects the HELM MMLU results to be placed in a folder structure that looks like this (schematically):

```text
helm_mmlu/
  runs/
    v1.0.0/
      <RUN_ID_1>/
        run_spec.json
        stats.json
      ...
```
The key point is: under helm_mmlu/runs/v1.0.0/ there should be many run directories (each a run id), and each run directory should contain at least run_spec.json and stats.json.
