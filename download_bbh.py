import os, json, gzip
import pandas as pd
from huggingface_hub import HfApi, hf_hub_download

repo = "open-llm-leaderboard/results"
rows = []

for f in HfApi(token=os.environ["HF_TOKEN"]).list_repo_files(repo, repo_type="dataset"):
    if not f.endswith((".json", ".jsonl", ".jsonl.gz", ".parquet")):
        continue

    path = hf_hub_download(repo, f, repo_type="dataset", token=os.environ["HF_TOKEN"])

    if f.endswith(".parquet"):
        records = pd.read_parquet(path).to_dict("records")
    else:
        opener = gzip.open if f.endswith(".gz") else open
        with opener(path, "rt") as fh:
            records = [json.loads(x) for x in fh if x.strip()]

    for ex in records:
        model = ex.get("model_name") or ex.get("model")
        for task, metrics in ex.get("results", {}).items():
            if task.startswith("leaderboard_bbh_") and task != "leaderboard_bbh":
                score = metrics.get("acc_norm,none")
                if score is not None:
                    rows.append((task.replace("leaderboard_", ""), model, score))

df = pd.DataFrame(rows, columns=["dataset_id", "model", "score"])
df = df.groupby(["dataset_id", "model"], as_index=False).mean()
df.to_csv("bbh_scores.csv", index=False)
